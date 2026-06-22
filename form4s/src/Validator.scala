package form4s

trait Validator[T] { that =>
  def validate(in: T): Seq[String]

  def toZIO: ValidatorZIO[T] =
    ValidatorZIO.fromPure(that)

  def contramap[U](f: U => T): Validator[U] =
    in => that.validate(f(in))
}

object Validator {
  def compose[T](validator: Validator[T]*): Validator[T] =
    in => validator.map(_.validate(in)).flatten

  def empty[T]: Validator[T] = _ => Seq.empty

  val nonEmpty: Validator[String] = in =>
    if (in.isEmpty()) Seq("Пустое поле запрещено") else Seq.empty

  val required: Validator[Option[?]] = in =>
    if (in.isEmpty) Seq("Обязательное поле") else Seq.empty

  def minLength(n: Int): Validator[String] = in =>
    if (in.length < n) Seq(s"Минимум $n символов") else Seq.empty

  def maxLength(n: Int): Validator[String] = in =>
    if (in.length > n) Seq(s"Максимум $n символов") else Seq.empty

  val isEmail: Validator[String] = in =>
    if (!in.contains("@")) Seq("Некорректный email") else Seq.empty

  val isPhone: Validator[String] = in =>
    if (!"""^\+\d{10,15}$""".r.matches(in)) Seq("Некорректный номер телефона")
    else Seq.empty

  def matches(regex: scala.util.matching.Regex): Validator[String] = in =>
    if (!regex.matches(in)) Seq("Неверный формат") else Seq.empty

  def min(n: Int): Validator[Int] = in =>
    if (in < n) Seq(s"Минимум $n") else Seq.empty

  def max(n: Int): Validator[Int] = in =>
    if (in > n) Seq(s"Максимум $n") else Seq.empty

  val requiredTrue: Validator[Boolean] = in =>
    if (!in) Seq("Требуется подтверждение") else Seq.empty
}
