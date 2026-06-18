package util

import zio.http.Form
import magnolia1.*

import zio.*
import zio.http.FormField
import zio.http.Body
import zio.http.Charsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

trait FormDecoder[T] {
  def decode(input: Form): Either[String, T]

  def isOptional: Boolean = false
}
object FormDecoder extends AutoDerivation[FormDecoder] {

  def decode[T: FormDecoder](input: Form): Either[String, T] =
    summon[FormDecoder[T]].decode(input)

  def decodeFormData[T: FormDecoder](input: Body): IO[Throwable, T] = {
    input.asMultipartForm
      .map { form => // convert all string like chunks to string
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
          case Left(error)  => ZIO.fail(new Exception(error))
          case Right(value) => ZIO.succeed(value)
      }
  }

  given stringDecoder: FormDecoder[String] = new FormDecoder[String] {
    def decode(input: Form): Either[String, String] =
      input.formData.head.stringValue.toRight(
        s"FormField ${input.formData.head.name} cannot be converted to primitive type(actual: ${input.formData.head.contentType})"
      )
  }

  given FormDecoder[Int] = new FormDecoder[Int] {
    def decode(input: Form): Either[String, Int] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          v.toIntOption.toRight(
            s"FormField ${input.formData.head.name} cannot be converted to Int(value: $v)"
          )
        )
  }

  given FormDecoder[Boolean] = new FormDecoder[Boolean] {
    def decode(input: Form): Either[String, Boolean] =
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
    def decode(input: Form): Either[String, LocalDateTime] =
      stringDecoder
        .decode(input)
        .flatMap(v =>
          try Right(LocalDateTime.parse(v, dateTimeFormatter))
          catch
            case e: Exception =>
              Right(LocalDateTime.parse(v + ":00", dateTimeFormatter))
        )
  }

  given [T](using decoder: FormDecoder[T]): FormDecoder[Option[T]] =
    new FormDecoder[Option[T]] {
      def decode(input: Form): Either[String, Option[T]] =
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
      def decode(input: Form): Either[String, Seq[T]] =
        if (input.formData.isEmpty) Right(Seq.empty)
        else {
          input.formData.head.stringValue match {
            case Some("")                   => Right(Seq.empty)
            case Some(v) if v.contains(",") =>
              val results =
                v.split(",")
                  .map(str => decoder.decode(Form(FormField.Simple("", str))))
              if (results.forall(_.isRight)) {
                Right(results.toSeq.flatMap(_.toOption))
              } else {
                val head = results.collect { case Left(err) =>
                  err
                }.head
                Left(head)
              }
            case _ =>
              val result =
                input.formData.map(field => decoder.decode(Form(field)))
              if (result.forall(_.isRight)) {
                Right(result.flatMap(_.toOption))
              } else {
                Left(result.collect { case Left(value) =>
                  value
                }.head)
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

  override def join[T](caseClass: CaseClass[FormDecoder, T]): FormDecoder[T] =
    new FormDecoder[T] {
      def decode(input: Form): Either[String, T] = {
        val decodedFields = caseClass.parameters.map { param =>
          val fieldName = param.label
          val decoder = param.typeclass
          val fields = input.formData.filter(_.name == fieldName)

          if (fields.nonEmpty) {
            decoder.decode(Form(fields))
          } else {
            if (decoder.isOptional) {
              decoder.decode(Form(FormField.Simple("", "")))
            } else {
              Left(s"No such field $fieldName on form")
            }
          }
        }

        decodedFields
          .foldLeft[Either[String, List[Any]]](Right(Nil)) {
            case (Right(acc), Right(value)) => Right(value :: acc)
            case (Left(err), _)             => Left(err)
            case (_, Left(err))             => Left(err)
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
      def decode(input: Form): Either[String, T] =
        Left(s"Cannot decode sealed trait")
    }

}
