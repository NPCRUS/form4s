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

/** Structured decode error for a single field.
  *
  * @param field
  *   dot-notation path to the field, e.g. `"address.city"` or `"docs.0.typ"`
  * @param message
  *   human-readable error message (Russian for built-in errors)
  */
case class DecodingError(field: String, message: String)

/** Typeclass for decoding `zio.http.Form` into a typed value `T`.
  *
  * Provides [[Magnolia]] auto-derivation for case classes via
  * [[AutoDerivation]]. Sealed traits are decoded by variant name when the trait
  * is a parameterless enum; other sealed traits produce an error.
  *
  * Built-in primitive decoders: `String`, `Int`, `Long`, `Double`, `Float`,
  * `BigDecimal`, `UUID`, `Boolean`, `LocalDateTime`. Container decoders:
  * `Option[T]`, `Seq[T]` (comma-separated or indexed dot-notation),
  * `Either[A, B]`.
  *
  * @note
  *   Built-in error messages are in Russian.
  * @tparam T
  *   the decoded type
  */
trait FormDecoder[T] { that =>

  /** Decode the form input, returning either a list of field errors or the
    * decoded value.
    */
  def decode(input: Form): Either[Seq[DecodingError], T]

  /** Whether this decoder treats missing input as a valid empty value. When
    * `true`, a missing field is decoded as the default empty value instead of
    * producing a "required field" error. Defaults to `false`.
    */
  def isOptional: Boolean = false

  /** Functor map: transform the decoded value. */
  def map[U](f: T => U): FormDecoder[U] = new FormDecoder[U] {
    def decode(input: Form): Either[Seq[DecodingError], U] =
      that.decode(input).map(f)
    override def isOptional: Boolean = that.isOptional
  }
}
object FormDecoder extends AutoDerivation[FormDecoder] {

  /** Summon a [[FormDecoder]] instance and run it. */
  def decode[T: FormDecoder](input: Form): Either[Seq[DecodingError], T] =
    summon[FormDecoder[T]].decode(input)

  /** Decode a `zio.http.Body` into a typed value. Handles multipart/form-data
    * bodies, treating `octet-stream` parts as UTF-8 text (common for
    * form-uploaded data without explicit charset).
    *
    * @return
    *   `IO[Throwable, T]` — decode errors are raised as an `Exception` (not as
    *   [[DecodingError]]).
    */
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

  /** Decodes the raw form value as a string. */
  given stringDecoder: FormDecoder[String] = new FormDecoder[String] {
    def decode(input: Form): Either[Seq[DecodingError], String] =
      input.formData.head.stringValue.toRight(
        Seq(DecodingError("", "Невозможно преобразовать в строку"))
      )
  }

  /** Decodes a string into `Int`. */
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

  /** Decodes a string into `Long`. */
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

  /** Decodes a string into `Double`. */
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

  /** Decodes a string into `Float`. */
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

  /** Decodes a string into `scala.math.BigDecimal`. */
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

  /** Decodes a string into `java.util.UUID`. */
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

  /** Decodes a checkbox value: `"on"`, `"true"`, `"1"` → `true`; `"off"`,
    * `"false"`, `"0"`, missing → `false`. Always optional.
    */
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

  /** Decodes a `LocalDateTime` from `yyyy-MM-dd'T'HH:mm` format; falls back to
    * appending `:00` seconds.
    */
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

  /** Decodes an optional field: empty string or missing → `None`. Always
    * optional.
    */
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

  /** Decodes a sequence of values. Supports comma-separated values in a single
    * field and indexed dot-notation (`docs.0.typ`, `docs.1.typ`). Always
    * optional (missing → empty seq).
    */
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
            val indexedForms = input.formData
              .groupBy { field =>
                field.name.split("\\.")(0).toIntOption.getOrElse(0)
              }
              .toSeq
              .sortBy(_._1)
              .map { (idx, fields) =>
                (
                  idx,
                  Form(fields.map(f => f.name(clearPathFromName(f.name))))
                )
              }

            val result = indexedForms.flatMap { (idx, form) =>
              (form.formData.map(_.name).toSet.size == 1) match {
                case true =>
                  form.formData.map(field =>
                    decoder
                      .decode(Form(field))
                      .left
                      .map(_.map(e => prependField(idx.toString, e)))
                  )
                case false =>
                  Seq(
                    decoder
                      .decode(form)
                      .left
                      .map(_.map(e => prependField(idx.toString, e)))
                  )
              }
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

  /** Tries decoding as `A` first, falling back to `B`. */
  given either[A, B](using
      decoderA: FormDecoder[A],
      decoderB: FormDecoder[B]
  ): FormDecoder[Either[A, B]] = input =>
    decoderA
      .decode(input)
      .map(Left.apply)
      .orElse(decoderB.decode(input).map(Right.apply))

  private def clearPathFromName(key: String): String =
    if (key.contains(".")) {
      key.substring(key.indexOf(".") + 1)
    } else if (key.contains("[")) {
      // TODO: support field[] maybe
      key
    } else {
      key
    }

  private def prependField(
      prefix: String,
      e: DecodingError
  ): DecodingError =
    e.copy(field =
      if (e.field.isEmpty) prefix else s"$prefix.${e.field}"
    )

  /** Magnolia auto-derivation for case classes. Decodes each parameter by
    * filtering form fields by the parameter name (supporting dot-notation
    * nesting), then constructs the case class.
    *
    * Missing non-optional fields produce a "required field" error.
    */
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
            .map(f => f.name(clearPathFromName(f.name)))

          if (fields.nonEmpty) {
            decoder
              .decode(Form(fields))
              .left
              .map(_.map(e => prependField(fieldName, e)))
          } else {
            if (decoder.isOptional) {
              decoder
                .decode(Form(FormField.Simple("", "")))
                .left
                .map(_.map(e => prependField(fieldName, e)))
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

  /** Magnolia auto-derivation for sealed traits. Supports parameterless enums
    * by matching the form value against variant names. Non-enum sealed traits
    * produce a decode error.
    */
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
