package form4s

import zio.*

/** Pure composable validator. Returns a (possibly empty) sequence of error
  * messages; empty means valid.
  */
trait Validator[A]:
  def validate(a: A): Seq[String]

  /** Concatenate with another validator for the same input type. */
  def ++(that: Validator[A]): Validator[A] = Validator.compose(this, that)

  def contramap[B](f: B => A): Validator[B] = b => validate(f(b))

  /** Transform the input before validation (identity by default; useful for
    * trimming/normalisation).
    */
  def map(f: A => A): Validator[A] = a => validate(f(a))

  /** Create a validator for [[Option]] that only checks the value when present.
    */
  def option: Validator[Option[A]] = opt => opt.fold(Seq.empty)(validate)

  /** Lift this pure validator into an effectful [[ValidatorZIO]]. */
  def toZIO: ValidatorZIO[A] = ValidatorZIO.fromPure(this)

object Validator:

  def compose[A](vs: Validator[A]*): Validator[A] =
    a => vs.flatMap(_.validate(a))

  def empty[A]: Validator[A] = _ => Seq.empty

  def custom[A](message: String)(predicate: A => Boolean): Validator[A] =
    a => if predicate(a) then Seq.empty else Seq(message)

  val nonEmpty: Validator[String] = in =>
    if in.isEmpty then Seq("Поле должно быть заполнено") else Seq.empty

  def nonEmptySeq[A]: Validator[Seq[A]] = in =>
    if in.isEmpty then Seq("Добавьте минимум одно значение") else Seq.empty

  def required[A]: Validator[Option[A]] = in =>
    if in.isEmpty then Seq("Обязательное поле") else Seq.empty

  def minLength(n: Int): Validator[String] = in =>
    if in.length < n then Seq(s"Минимум $n символов") else Seq.empty

  def maxLength(n: Int): Validator[String] = in =>
    if in.length > n then Seq(s"Максимум $n символов") else Seq.empty

  val isEmail: Validator[String] = in =>
    if """^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$""".r.matches(in) then Seq.empty
    else Seq("Некорректный email")

  val isPhone: Validator[String] = in =>
    if """^\+\d{10,15}$""".r.matches(in) then Seq.empty
    else Seq("Некорректный номер телефона")

  def matches(regex: scala.util.matching.Regex): Validator[String] = in =>
    if regex.matches(in) then Seq.empty else Seq("Неверный формат")

  def min(n: Int): Validator[Int] = in =>
    if in < n then Seq(s"Минимум $n") else Seq.empty

  def max(n: Int): Validator[Int] = in =>
    if in > n then Seq(s"Максимум $n") else Seq.empty

  val requiredTrue: Validator[Boolean] = in =>
    if in then Seq.empty else Seq("Требуется подтверждение")

  val positive: Validator[Int] = in =>
    if in > 0 then Seq.empty else Seq("Значение должно быть положительным")

  val isUrl: Validator[String] = in =>
    if """^https?://[^\s]+""".r.matches(in) then Seq.empty
    else Seq("Некорректный URL")

/** A validation rule: run over the whole model, producing an effect that
  * resolves to a map of error path → messages.
  */
final case class Rule[S](
    validate: S => ZIO[Any, Nothing, Map[String, Seq[String]]]
)

/** A field-level rule: validate the value focused by `field`, keyed by the
  * field's name (derived from the lens).
  */
def rule[S, A](field: Field[S, A], v: ValidatorZIO[A]): Rule[S] =
  Rule(s =>
    v.validate(field.get(s)).map { errs =>
      if errs.nonEmpty then Map(field.name -> errs) else Map.empty
    }
  )

def rule[S, A](field: Field[S, A], v: Validator[A]): Rule[S] =
  rule(field, v.toZIO)

/** A whole-model rule (e.g. cross-field consistency), optionally keyed at a
  * `path` (default: the root form).
  */
def whole[S](v: ValidatorZIO[S], path: String): Rule[S] =
  Rule(s =>
    v.validate(s).map { errs =>
      if errs.nonEmpty then Map(path -> errs) else Map.empty
    }
  )

def whole[S](v: Validator[S], path: String = ""): Rule[S] =
  whole(v.toZIO, path)

/** A repeated-subform rule: validate every element of a `Seq` field with a
  * sub-[[Validation]], prefixing error keys with `field.INDEX.`.
  */
def ruleEach[S, A](
    field: Field[S, Seq[A]],
    validation: Validation[A]
): Rule[S] =
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

/** A set of rules, run over a model to produce a path → errors map. */
final case class Validation[S](rules: List[Rule[S]]):
  def ++(that: Validation[S]): Validation[S] =
    Validation(rules ++ that.rules)

  def run(s: S): ZIO[Any, Nothing, Map[String, Seq[String]]] =
    ZIO
      .collectAllPar(rules.map(_.validate(s)))
      .map(_.foldLeft(Map.empty[String, Seq[String]])(mergeErrors))

object Validation:
  def apply[S](rules: Rule[S]*): Validation[S] = Validation(rules.toList)

private def mergeErrors(
    acc: Map[String, Seq[String]],
    m: Map[String, Seq[String]]
): Map[String, Seq[String]] =
  m.foldLeft(acc) { case (a, (k, errs)) =>
    a.updated(k, a.getOrElse(k, Seq.empty) ++ errs)
  }
