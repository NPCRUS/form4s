package form4s

/** Pure composable validator. Validates a value of type `T` and returns a
  * (possibly empty) sequence of error messages. An empty sequence means the
  * value is valid.
  *
  * Combine validators with [[compose]], [[contramap]] to adapt input types,
  * [[map]] for input normalisation, and [[option]] for optional fields. Lift to
  * an effectful [[ValidatorZIO]] via [[toZIO]].
  *
  * @note
  *   All built-in error messages are in Russian.
  * @tparam T
  *   the type of value being validated
  */
trait Validator[T] { that =>

  /** Run the validation, returning error messages (empty = valid). */
  def validate(in: T): Seq[String]

  /** Lift this pure validator into an effectful [[ValidatorZIO]]. */
  def toZIO: ValidatorZIO[T] =
    ValidatorZIO.fromPure(that)

  /** Adapt this validator to a different input type by applying `f` first. */
  def contramap[U](f: U => T): Validator[U] =
    in => that.validate(f(in))

  /** Transform the input before validation (identity by default; useful for
    * trimming/normalisation).
    */
  def map(f: T => T): Validator[T] =
    in => that.validate(f(in))

  /** Create a validator for [[Option]] that only checks the value when present.
    */
  def option: Validator[Option[T]] =
    opt => opt.fold(Seq.empty)(that.validate)
}

object Validator {

  /** Compose multiple validators, concatenating all their error messages. */
  def compose[T](validator: Validator[T]*): Validator[T] =
    in => validator.map(_.validate(in)).flatten

  /** A validator that always passes (returns no errors). */
  def empty[T]: Validator[T] = _ => Seq.empty

  /** Fails if the string is empty. */
  val nonEmpty: Validator[String] = in =>
    if (in.isEmpty()) Seq("Поле должно быть заполнено") else Seq.empty

  /** Fails if the [[Option]] is `None`. */
  val required: Validator[Option[?]] = in =>
    if (in.isEmpty) Seq("Обязательное поле") else Seq.empty

  /** Fails if the string has fewer than `n` characters. */
  def minLength(n: Int): Validator[String] = in =>
    if (in.length < n) Seq(s"Минимум $n символов") else Seq.empty

  /** Fails if the string has more than `n` characters. */
  def maxLength(n: Int): Validator[String] = in =>
    if (in.length > n) Seq(s"Максимум $n символов") else Seq.empty

  /** Validates that the string looks like an email address. */
  val isEmail: Validator[String] = in =>
    if (!"""^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$""".r.matches(in))
      Seq("Некорректный email")
    else Seq.empty

  /** Validates that the string is a phone number starting with `+` and 10–15
    * digits.
    */
  val isPhone: Validator[String] = in =>
    if (!"""^\+\d{10,15}$""".r.matches(in)) Seq("Некорректный номер телефона")
    else Seq.empty

  /** Fails if the string does not match the given regex. */
  def matches(regex: scala.util.matching.Regex): Validator[String] = in =>
    if (!regex.matches(in)) Seq("Неверный формат") else Seq.empty

  /** Fails if the integer is less than `n`. */
  def min(n: Int): Validator[Int] = in =>
    if (in < n) Seq(s"Минимум $n") else Seq.empty

  /** Fails if the integer is greater than `n`. */
  def max(n: Int): Validator[Int] = in =>
    if (in > n) Seq(s"Максимум $n") else Seq.empty

  /** Fails if the boolean is `false` (e.g. unchecked checkbox). */
  val requiredTrue: Validator[Boolean] = in =>
    if (!in) Seq("Требуется подтверждение") else Seq.empty

  /** Fails if the integer is zero or negative. */
  val positive: Validator[Int] = in =>
    if (in > 0) Seq.empty else Seq("Значение должно быть положительным")

  /** Validates that the string looks like an HTTP/HTTPS URL. */
  val isUrl: Validator[String] = in =>
    if ("""^https?://[^\s]+""".r.matches(in)) Seq.empty
    else Seq("Некорректный URL")

  /** Create a custom validator with the given error message and predicate.
    * Passes when the predicate returns `true`.
    */
  def custom[T](message: String)(predicate: T => Boolean): Validator[T] =
    in => if (predicate(in)) Seq.empty else Seq(message)
}
