package form4s2

import zio.http.{Form, FormField}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/** Structured decode error for a single field. */
final case class DecodingError(field: String, message: String)

/** Simple typeclass decoding a single raw string into a value. Every leaf field
  * requires one, and it is summoned implicitly.
  */
trait FieldDecoder[A]:
  def decode(raw: String): Either[String, A]

object FieldDecoder:

  given FieldDecoder[String] = s => Right(s)

  given FieldDecoder[Int] = s =>
    s.toIntOption.toRight("Невозможно преобразовать в число")

  given FieldDecoder[Long] = s =>
    s.toLongOption.toRight("Невозможно преобразовать в число")

  given FieldDecoder[Double] = s =>
    s.toDoubleOption.toRight("Невозможно преобразовать в число")

  given FieldDecoder[Float] = s =>
    s.toFloatOption.toRight("Невозможно преобразовать в число")

  given FieldDecoder[BigDecimal] = s =>
    try Right(BigDecimal(s))
    catch case _: Exception => Left("Невозможно преобразовать в число")

  given FieldDecoder[UUID] = s =>
    try Right(UUID.fromString(s))
    catch
      case _: IllegalArgumentException =>
        Left("Невозможно преобразовать в UUID")

  given FieldDecoder[Boolean] = s =>
    s.toLowerCase match
      case "on" | "true" | "1"   => Right(true)
      case "off" | "false" | "0" => Right(false)
      case _                     => Right(false)

  private val dateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

  given FieldDecoder[LocalDateTime] = s =>
    try Right(LocalDateTime.parse(s, dateTimeFormatter))
    catch
      case _: Exception =>
        try Right(LocalDateTime.parse(s + ":00", dateTimeFormatter))
        catch case _: Exception => Left("Невозможно преобразовать дату/время")

  given optionDecoder[A](using dec: FieldDecoder[A]): FieldDecoder[Option[A]] =
    s => if s.isEmpty then Right(None) else dec.decode(s).map(Some(_))

  given seqDecoder[A](using dec: FieldDecoder[A]): FieldDecoder[Seq[A]] =
    s =>
      if s.isEmpty then Right(Seq.empty)
      else
        s.split(",").toList match
          case parts =>
            val results = parts.map(dec.decode)
            if results.forall(_.isRight) then Right(results.flatMap(_.toOption))
            else Left(results.collect { case Left(e) => e }.mkString("; "))

/** Structural typeclass decoding a `zio.http.Form` into a typed value. Derived
  * via `Mirror` for case classes; uses [[FieldDecoder]] for leaf fields.
  */
trait FormDecoder[A]:
  def decode(form: Form): Either[Seq[DecodingError], A]
  def isOptional: Boolean = false

object FormDecoder:

  /** Build a leaf decoder from a [[FieldDecoder]] over the first string value.
    */
  private def leaf[A: FieldDecoder]: FormDecoder[A] = new FormDecoder[A]:
    def decode(form: Form): Either[Seq[DecodingError], A] =
      form.formData.headOption.flatMap(_.stringValue) match
        case Some(v) =>
          summon[FieldDecoder[A]]
            .decode(v)
            .left
            .map(m => Seq(DecodingError("", m)))
        case None => Left(Seq(DecodingError("", "Обязательное поле")))

  given stringDecoder: FormDecoder[String] = leaf[String]
  given intDecoder: FormDecoder[Int] = leaf[Int]
  given longDecoder: FormDecoder[Long] = leaf[Long]
  given doubleDecoder: FormDecoder[Double] = leaf[Double]
  given floatDecoder: FormDecoder[Float] = leaf[Float]
  given bigDecimalDecoder: FormDecoder[BigDecimal] = leaf[BigDecimal]
  given uuidDecoder: FormDecoder[UUID] = leaf[UUID]
  given localDateTimeDecoder: FormDecoder[LocalDateTime] = leaf[LocalDateTime]

  given booleanDecoder: FormDecoder[Boolean] = new FormDecoder[Boolean]:
    def decode(form: Form): Either[Seq[DecodingError], Boolean] =
      form.formData.headOption.flatMap(_.stringValue) match
        case Some(v) =>
          summon[FieldDecoder[Boolean]]
            .decode(v)
            .left
            .map(m => Seq(DecodingError("", m)))
        case None => Right(false)
    override def isOptional: Boolean = true

  given optionDecoder[A](using dec: FormDecoder[A]): FormDecoder[Option[A]] =
    new FormDecoder[Option[A]]:
      def decode(form: Form): Either[Seq[DecodingError], Option[A]] =
        if form.formData.isEmpty then Right(None)
        else
          form.formData.head match
            case FormField.Simple(_, "") => Right(None)
            case _                       => dec.decode(form).map(Some(_))
      override def isOptional: Boolean = true

  given seqDecoder[A](using dec: FormDecoder[A]): FormDecoder[Seq[A]] =
    new FormDecoder[Seq[A]]:
      def decode(form: Form): Either[Seq[DecodingError], Seq[A]] =
        if form.formData.isEmpty then Right(Seq.empty)
        else
          form.formData.toSeq match
            case Seq(field) =>
              field.stringValue match
                case Some("")                   => Right(Seq.empty)
                case Some(v) if v.contains(",") =>
                  val results =
                    v.split(",")
                      .map(s => dec.decode(Form(FormField.Simple("", s))))
                  if results.forall(_.isRight) then
                    Right(results.toSeq.flatMap(_.toOption))
                  else Left(results.collect { case Left(e) => e }.flatten.toSeq)
                case _ => decodeIndexed(form)
            case _ => decodeIndexed(form)

      private def decodeIndexed(
          form: Form
      ): Either[Seq[DecodingError], Seq[A]] =
        val indexed = form.formData
          .groupBy(f => f.name.split("\\.")(0).toIntOption.getOrElse(0))
          .toSeq
          .sortBy(_._1)
          .map { (idx, fields) =>
            (idx, Form(fields.map(f => f.name(clearPathFromName(f.name)))))
          }
        val result = indexed.flatMap { (idx, sub) =>
          if sub.formData.map(_.name).toSet.size == 1 then
            sub.formData.map(f =>
              dec
                .decode(Form(f))
                .left
                .map(_.map(e => prependField(idx.toString, e)))
            )
          else
            Seq(
              dec
                .decode(sub)
                .left
                .map(_.map(e => prependField(idx.toString, e)))
            )
        }
        if result.forall(_.isRight) then Right(result.flatMap(_.toOption))
        else Left(result.collect { case Left(e) => e }.flatten.toSeq)

      override def isOptional: Boolean = true

  /** Mirror-based auto-derivation for case classes. */
  inline given derived[A <: Product](using
      m: Mirror.ProductOf[A]
  ): FormDecoder[A] =
    new FormDecoder[A]:
      def decode(form: Form): Either[Seq[DecodingError], A] =
        val names = constValueTuple[m.MirroredElemLabels].toList
          .asInstanceOf[List[String]]
        val decoders = summonAll[m.MirroredElemTypes]
        val decodedFields = names.zip(decoders).map { (fieldName, dec) =>
          val fields = form.formData
            .filter(f =>
              f.name == fieldName || f.name.startsWith(fieldName + ".")
            )
            .map(f => f.name(clearPathFromName(f.name)))
          if fields.nonEmpty then
            dec
              .decode(Form(fields))
              .left
              .map(_.map(e => prependField(fieldName, e)))
          else if dec.isOptional then
            dec
              .decode(Form(FormField.Simple("", "")))
              .left
              .map(_.map(e => prependField(fieldName, e)))
          else Left(Seq(DecodingError(fieldName, "Обязательное поле")))
        }

        val combined =
          decodedFields.foldLeft[Either[Seq[DecodingError], List[Any]]](
            Right(Nil)
          ) {
            case (Right(acc), Right(v)) => Right(v :: acc)
            case (Left(errs), Right(_)) => Left(errs)
            case (Right(_), Left(errs)) => Left(errs)
            case (Left(e1), Left(e2))   => Left(e1 ++ e2)
          }

        combined.map(values =>
          m.fromProduct(Tuple.fromArray(values.reverse.toArray))
        )

      override def isOptional: Boolean = true

  inline def summonAll[T <: Tuple]: List[FormDecoder[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[FormDecoder[h]]
          .asInstanceOf[FormDecoder[Any]] :: summonAll[t]

  private def clearPathFromName(key: String): String =
    if key.contains(".") then key.substring(key.indexOf(".") + 1) else key

  private def prependField(prefix: String, e: DecodingError): DecodingError =
    e.copy(field = if e.field.isEmpty then prefix else s"$prefix.${e.field}")

  /** Convenience summon-and-run. */
  def decode[A: FormDecoder](form: Form): Either[Seq[DecodingError], A] =
    summon[FormDecoder[A]].decode(form)
