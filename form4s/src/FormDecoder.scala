package form4s

import zio.http.Form
import magnolia1.*

import zio.*
import zio.http.FormField
import zio.http.Body
import zio.http.Charsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.math.BigDecimal

case class DecodingError(field: String, message: String)

trait FormDecoder[T] { that =>
  def decode(input: Form): Either[Seq[DecodingError], T]

  def isOptional: Boolean = false

  def map[U](f: T => U): FormDecoder[U] = new FormDecoder[U] {
    def decode(input: Form): Either[Seq[DecodingError], U] =
      that.decode(input).map(f)
    override def isOptional: Boolean = that.isOptional
  }
}
object FormDecoder extends AutoDerivation[FormDecoder] {

  def decode[T: FormDecoder](input: Form): Either[Seq[DecodingError], T] =
    summon[FormDecoder[T]].decode(input)

  def decodeFormData[T: FormDecoder](input: Body): IO[Throwable, T] = {
    input.asMultipartForm
      .map { form =>
        val newChunks = form.formData.map {
          case FormField.Binary(name, data, contentType, _, _)
              if contentType.subType == "octet-stream" =>
            FormField.Simple(name, new String(data.toArray, Charsets.Utf8))
          case rest => rest
        }
        form.copy(formData = newChunks)
      }
      .flatMap { form =>
        FormDecoder.decode[T](form) match
          case Left(errors) =>
            ZIO.fail(
              new Exception(
                errors.map(e => s"${e.field}: ${e.message}").mkString("; ")
              )
            )
          case Right(value) => ZIO.succeed(value)
      }
  }

  given stringDecoder: FormDecoder[String] = new FormDecoder[String] {
    def decode(input: Form): Either[Seq[DecodingError], String] =
      input.formData.head.stringValue.toRight(
        Seq(DecodingError("", "Невозможно преобразовать в строку"))
      )
  }

  given FormDecoder[Int] = new FormDecoder[Int] {
    def decode(input: Form): Either[Seq[DecodingError], Int] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          v.toIntOption.toRight(
            Seq(DecodingError("", "Невозможно преобразовать в число"))
          )
        )
  }

  given FormDecoder[Long] = new FormDecoder[Long] {
    def decode(input: Form): Either[Seq[DecodingError], Long] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          v.toLongOption.toRight(
            Seq(DecodingError("", "Невозможно преобразовать в число"))
          )
        )
  }

  given FormDecoder[Double] = new FormDecoder[Double] {
    def decode(input: Form): Either[Seq[DecodingError], Double] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          v.toDoubleOption.toRight(
            Seq(DecodingError("", "Невозможно преобразовать в число"))
          )
        )
  }

  given FormDecoder[Float] = new FormDecoder[Float] {
    def decode(input: Form): Either[Seq[DecodingError], Float] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          v.toFloatOption.toRight(
            Seq(DecodingError("", "Невозможно преобразовать в число"))
          )
        )
  }

  given FormDecoder[BigDecimal] = new FormDecoder[BigDecimal] {
    def decode(input: Form): Either[Seq[DecodingError], BigDecimal] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          try Right(BigDecimal(v))
          catch
            case _: Exception =>
              Left(Seq(DecodingError("", "Невозможно преобразовать в число")))
        )
  }

  given FormDecoder[UUID] = new FormDecoder[UUID] {
    def decode(input: Form): Either[Seq[DecodingError], UUID] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          try Right(UUID.fromString(v))
          catch
            case _: IllegalArgumentException =>
              Left(Seq(DecodingError("", "Невозможно преобразовать в UUID")))
        )
  }

  given FormDecoder[Boolean] = new FormDecoder[Boolean] {
    def decode(input: Form): Either[Seq[DecodingError], Boolean] =
      stringDecoder
        .decode(input)
        .flatMap { v =>
          v.toLowerCase match {
            case "on" | "true" | "1"   => Right(true)
            case "off" | "false" | "0" => Right(false)
            case _                     => Right(false)
          }
        }

    override def isOptional: Boolean = true
  }

  private val dateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

  given FormDecoder[LocalDateTime] = new FormDecoder[LocalDateTime] {
    def decode(input: Form): Either[Seq[DecodingError], LocalDateTime] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          try Right(LocalDateTime.parse(v, dateTimeFormatter))
          catch
            case _: Exception =>
              try Right(LocalDateTime.parse(v + ":00", dateTimeFormatter))
              catch
                case _: Exception =>
                  Left(
                    Seq(
                      DecodingError("", "Невозможно преобразовать дату/время")
                    )
                  )
        )
  }

  given [T](using decoder: FormDecoder[T]): FormDecoder[Option[T]] =
    new FormDecoder[Option[T]] {
      def decode(input: Form): Either[Seq[DecodingError], Option[T]] =
        if (input.formData.isEmpty) Right(None)
        else
          input.formData.head match {
            case FormField.Simple(_, value) =>
              value match {
                case "" => Right(None)
                case _  => decoder.decode(input).map(Some(_))
              }
            case _ => decoder.decode(input).map(Some(_))
          }

      override def isOptional: Boolean = true
    }

  given [T](using decoder: FormDecoder[T]): FormDecoder[Seq[T]] =
    new FormDecoder[Seq[T]] {
      def decode(input: Form): Either[Seq[DecodingError], Seq[T]] =
        if (input.formData.isEmpty) Right(Seq.empty)
        else {
          (input.formData.toSeq match {
            case Seq(field) =>
              field.stringValue match {
                case Some("")                   => Some(Right(Seq.empty))
                case Some(v) if v.contains(",") =>
                  val results =
                    v.split(",")
                      .map(str =>
                        decoder.decode(Form(FormField.Simple("", str)))
                      )
                  if (results.forall(_.isRight)) {
                    Some(Right(results.toSeq.flatMap(_.toOption)))
                  } else {
                    Some(
                      Left(
                        results
                          .collect { case Left(errs) => errs }
                          .flatten
                          .toSeq
                      )
                    )
                  }
                case _ => None
              }
            case _ => None
          }).getOrElse {
            val forms = input.formData
              .groupBy { field =>
                field.name.split(".")(0).toIntOption.getOrElse(0)
              }
              .mapValues(Form(_*))
              .values
              .toSeq

            val result = forms match {
              case Seq(form) =>
                form.formData.map(field => decoder.decode(Form(field)))
              case seq =>
                seq.map(form => decoder.decode(form))
            }

            if (result.forall(_.isRight)) {
              Right(result.flatMap(_.toOption))
            } else {
              Left(result.collect { case Left(errs) => errs }.flatten.toSeq)
            }
          }
        }

      override def isOptional: Boolean = true
    }

  given either[A, B](using
      decoderA: FormDecoder[A],
      decoderB: FormDecoder[B]
  ): FormDecoder[Either[A, B]] = input =>
    decoderA
      .decode(input)
      .map(Left.apply)
      .orElse(decoderB.decode(input).map(Right.apply))

  private def clearPathFromName(fieldName: String, key: String): String =
    if (key.contains(".")) {
      key.substring(key.indexOf(".") + 1)
    } else if (key.contains("[")) {
      // TODO: support field[] maybe
      key
    } else {
      key
    }

  override def join[T](caseClass: CaseClass[FormDecoder, T]): FormDecoder[T] =
    new FormDecoder[T] {
      def decode(input: Form): Either[Seq[DecodingError], T] = {
        val decodedFields = caseClass.parameters.map { param =>
          val fieldName = param.label
          val decoder = param.typeclass
          val fields = input.formData
            .filter(f =>
              f.name == fieldName || f.name.startsWith(fieldName + ".")
            )
            .map(f => f.name(clearPathFromName(fieldName, f.name)))

          if (fields.nonEmpty) {
            decoder
              .decode(Form(fields))
              .left
              .map(_.map(_.copy(field = fieldName)))
          } else {
            if (decoder.isOptional) {
              decoder
                .decode(Form(FormField.Simple("", "")))
                .left
                .map(_.map(_.copy(field = fieldName)))
            } else {
              Left(Seq(DecodingError(fieldName, "Обязательное поле")))
            }
          }
        }

        decodedFields
          .foldLeft[Either[Seq[DecodingError], List[Any]]](Right(Nil)) {
            case (Right(acc), Right(value)) => Right(value :: acc)
            case (Left(errs), Right(_))     => Left(errs)
            case (Right(_), Left(errs))     => Left(errs)
            case (Left(errs1), Left(errs2)) => Left(errs1 ++ errs2)
          }
          .map { values =>
            caseClass.rawConstruct(values.reverse)
          }
      }
    }

  override def split[T](
      sealedTrait: SealedTrait[FormDecoder, T]
  ): FormDecoder[T] =
    new FormDecoder[T] {
      def decode(input: Form): Either[Seq[DecodingError], T] =
        if (sealedTrait.isEnum) {
          input.formData.headOption.flatMap(field =>
            sealedTrait.subtypes.find(
              _.typeInfo.short == field.stringValue.getOrElse("")
            )
          ) match
            case Some(value) =>
              value.typeclass.decode(input)
            case None =>
              Left(Seq(DecodingError("", "Нет такого варианта выбора")))
        } else {
          Left(Seq(DecodingError("", "Невозможно декодировать ast")))
        }
    }

}
