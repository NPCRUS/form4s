package form4s

import zio.*

/** Pure composable validator. Returns a (possibly empty) sequence of error
  * messages; empty means valid.
  *
  * This is the leaf-level building block for [[Validation]] rules; it is
  * effect-free. Lift it to [[ValidatorZIO]] with [[toZIO]] (which the
  * [[rule]]/[[whole]] combinators do automatically).
  *
  * @tparam A
  *   the type of value being validated
  */
trait Validator[A]:
  def validate(a: A): Seq[String]

  /** Concatenate with another validator for the same input type. */
  def ++(that: Validator[A]): Validator[A] = Validator.compose(this, that)

  /** Adapt this validator to a different input type by applying `f` first. */
  def contramap[B](f: B => A): Validator[B] = b => validate(f(b))

  /** Transform the input before validation (identity by default; useful for
    * trimming/normalisation).
    */
  def map(f: A => A): Validator[A] = a => validate(f(a))

  /** Lift to [[Option]]: only checks the value when it is `Some`. */
  def option: Validator[Option[A]] = opt => opt.fold(Seq.empty)(validate)

  /** Lift this pure validator into an effectful [[ValidatorZIO]] (requiring no
    * environment).
    */
  def toZIO: ValidatorZIO[A, Any] = ValidatorZIO.fromPure(this)

object Validator:

  /** Combine validators, concatenating all their error messages. */
  def compose[A](vs: Validator[A]*): Validator[A] =
    a => vs.flatMap(_.validate(a))

  /** A validator that always passes. */
  def empty[A]: Validator[A] = _ => Seq.empty

  /** Create a validator that fails with `message` when `predicate` is false. */
  def custom[A](message: String)(predicate: A => Boolean): Validator[A] =
    a => if predicate(a) then Seq.empty else Seq(message)

  /** Fails on an empty string. */
  val nonEmpty: Validator[String] = in =>
    if in.isEmpty then Seq("Поле должно быть заполнено") else Seq.empty

  /** Fails on an empty sequence. */
  def nonEmptySeq[A]: Validator[Seq[A]] = in =>
    if in.isEmpty then Seq("Добавьте минимум одно значение") else Seq.empty

  /** Fails when the [[Option]] is `None`. */
  def required[A]: Validator[Option[A]] = in =>
    if in.isEmpty then Seq("Обязательное поле") else Seq.empty

  /** Fails when the string has fewer than `n` characters. */
  def minLength(n: Int): Validator[String] = in =>
    if in.length < n then Seq(s"Минимум $n символов") else Seq.empty

  /** Fails when the string has more than `n` characters. */
  def maxLength(n: Int): Validator[String] = in =>
    if in.length > n then Seq(s"Максимум $n символов") else Seq.empty

  /** Fails when the string does not look like an email address. */
  val isEmail: Validator[String] = in =>
    if """^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$""".r.matches(in) then Seq.empty
    else Seq("Некорректный email")

  /** Fails when the string is not a `+`-prefixed phone number. */
  val isPhone: Validator[String] = in =>
    if """^\+\d{10,15}$""".r.matches(in) then Seq.empty
    else Seq("Некорректный номер телефона")

  /** Fails when the string does not fully match `regex`. */
  def matches(regex: scala.util.matching.Regex): Validator[String] = in =>
    if regex.matches(in) then Seq.empty else Seq("Неверный формат")

  /** Fails when the integer is less than `n`. */
  def min(n: Int): Validator[Int] = in =>
    if in < n then Seq(s"Минимум $n") else Seq.empty

  /** Fails when the integer is greater than `n`. */
  def max(n: Int): Validator[Int] = in =>
    if in > n then Seq(s"Максимум $n") else Seq.empty

  /** Fails when the boolean is `false` (e.g. unchecked checkbox). */
  val requiredTrue: Validator[Boolean] = in =>
    if in then Seq.empty else Seq("Требуется подтверждение")

  /** Fails when the integer is not strictly positive. */
  val positive: Validator[Int] = in =>
    if in > 0 then Seq.empty else Seq("Значение должно быть положительным")

  /** Fails when the string is not an `http(s)` URL. */
  val isUrl: Validator[String] = in =>
    if """^https?://[^\s]+""".r.matches(in) then Seq.empty
    else Seq("Некорректный URL")

/** A validation rule: run over the whole model, producing an effect that
  * resolves to a map of error path → messages.
  *
  * `R` is contravariant, so a pure rule (`R = Any`) auto-widens into any
  * environment.
  */
final case class Rule[S, -R](
    validate: S => ZIO[R, Nothing, Map[String, Seq[String]]]
)

/** A field-level rule: validate the value focused by `field`, keyed by the
  * field's name (derived from the lens).
  *
  * Accepts a pure [[Validator]] (auto-lifts) or an effectful [[ValidatorZIO]].
  */
def rule[S, A, R](field: Field[S, A], v: ValidatorZIO[A, R]): Rule[S, R] =
  Rule(s =>
    v.validate(field.get(s)).map { errs =>
      if errs.nonEmpty then Map(field.name -> errs) else Map.empty
    }
  )

def rule[S, A](field: Field[S, A], v: Validator[A]): Rule[S, Any] =
  rule(field, v.toZIO)

/** A whole-model rule (e.g. cross-field consistency), keyed at `path` (default:
  * the root form). The pure [[Validator]] overload auto-lifts to
  * [[ValidatorZIO]].
  */
def whole[S, R](v: ValidatorZIO[S, R], path: String): Rule[S, R] =
  Rule(s =>
    v.validate(s).map { errs =>
      if errs.nonEmpty then Map(path -> errs) else Map.empty
    }
  )

def whole[S](v: Validator[S], path: String = ""): Rule[S, Any] =
  whole(v.toZIO, path)

/** A repeated-subform rule: validate every element of a `Seq` field with a
  * sub-[[Validation]], prefixing error keys with `field.INDEX.` (e.g.
  * `docs.0.number`).
  */
def ruleEach[S, A, R](
    field: Field[S, Seq[A]],
    validation: Validation[A, R]
): Rule[S, R] =
  Rule(s =>
    ZIO
      .collectAllPar(
        field.get(s).zipWithIndex.map { (a, i) =>
          validation.run(a).map { m =>
            m.map { case (k, errs) => (s"${field.name}.$i.$k", errs) }
          }
        }
      )
      .map(_.foldLeft(Map.empty[String, Seq[String]])(mergeErrors))
  )

/** A set of [[Rule]]s, run over a model to produce a path → errors map. Rules
  * run concurrently and pure validators auto-lift to ZIO.
  *
  * {{{
  * import zio.blocks.schema.Schema
  *
  * case class Order(email: String, items: Seq[Item])
  * object Order extends AutoFields[Order]:
  *   given Schema[Order] = Schema.derived
  *
  * case class Item(sku: String)
  * object Item extends AutoFields[Item]:
  *   given Schema[Item] = Schema.derived
  *
  * val v: Validation[Order, Any] = Validation(
  *   rule(Order.email, Validator.isEmail),                 // pure, auto-lifts
  *   whole(Validator.custom[Order]("need items")(_.items.nonEmpty), "items"),
  *   ruleEach(Order.items, Validation(rule(Item.sku, Validator.nonEmpty)))
  * )
  * }}}
  *
  * @see
  *   [[decodeAndValidate]] runs this over a decoded form.
  */
final case class Validation[S, -R](rules: List[Rule[S, R]]):
  def ++[R1 <: R](that: Validation[S, R1]): Validation[S, R1] =
    Validation(rules ++ that.rules)

  def run(s: S): ZIO[R, Nothing, Map[String, Seq[String]]] =
    ZIO
      .collectAllPar(rules.map(_.validate(s)))
      .map(_.foldLeft(Map.empty[String, Seq[String]])(mergeErrors))

object Validation:
  def apply[S, R](rules: Rule[S, R]*): Validation[S, R] = Validation(
    rules.toList
  )

private def mergeErrors(
    acc: Map[String, Seq[String]],
    m: Map[String, Seq[String]]
): Map[String, Seq[String]] =
  m.foldLeft(acc) { case (a, (k, errs)) =>
    a.updated(k, a.getOrElse(k, Seq.empty) ++ errs)
  }
