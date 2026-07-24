package form4s

import zio.ZIO
import scala.deriving.Mirror
import scala.compiletime.summonInline

trait FormV2[Elem] {
  def base: Elem
  def amend(p: Elem)(inside: Elem*): Elem
  def render(in: Elem): String

  trait Renderable[T] { that =>
    def draw(
        schema: FieldSchema[T],
        fieldName: String,
        oldValue: Option[T],
        errors: Seq[String]
    ): Elem

    def optional: Renderable[Option[T]] = new Renderable[Option[T]] {
      def draw(
          schema: FieldSchema[Option[T]],
          fieldName: String,
          oldValue: Option[Option[T]],
          errors: Seq[String]
      ): Elem =
        that.draw(
          schema.asInstanceOf[FieldSchema[T]],
          fieldName,
          oldValue.flatten,
          errors
        )
    }
  }

  enum FormSchema[T] {
    case Field[T](schema: FieldSchema[T]) extends FormSchema[T]
    case SubForm[F[B[_]] <: Product](
        label: String,
        form: F[FormSchema]
    ) extends FormSchema[F[FormSchema]]
    case RepeatedSubForm[F[B[_]] <: Product](
        label: String,
        form: F[FormSchema]
    ) extends FormSchema[Seq[F[FormSchema]]]
  }

  case class FieldSchema[T](
      label: String,
      renderer: Renderable[T],
      placeholderAttr: String,
      typeAttr: String,
      validator: ValidatorZIO[T] = Validator.empty[T].toZIO,
      options: Seq[String] = Seq.empty
  )

  def subFormContainer(label: String): Elem

  def addBtn: Elem
  def deleteBtn: Elem
  def listOfSubformsContainer(
      label: String,
      fieldName: String,
      items: Seq[Elem],
      templateItem: Elem
  ): Elem

  def draw[T[F[_]] <: Product](
      oldValue: Option[T[[T] =>> T]],
      errors: Map[String, Seq[String]]
  )(using
      schema: T[FormSchema]
  ): Elem = {
    val oldValues = oldValue.map(_.productIterator.toSeq)
    val names = schema.productElementNames.toSeq
    amend(base)(
      schema.productIterator.zipWithIndex.map { (schemaAny, idx) =>
        type Gen
        val schema = schemaAny.asInstanceOf[FormSchema[Gen]]
        schema match
          case FormSchema.Field(schema) =>
            schema.renderer.draw(
              schema,
              names(idx),
              oldValues.map(v => v(idx).asInstanceOf[Gen]),
              errors.get(names(idx)).getOrElse(Seq.empty)
            )
          case FormSchema.SubForm(label, form) =>
            amend(subFormContainer(label))(
              draw(None, Map.empty)(using form)
            )
          case FormSchema.RepeatedSubForm(label, form) =>
            val count =
              oldValues.map(v => v(idx).asInstanceOf[Seq[?]].size).getOrElse(0)
            val existing = (0 until count).map { _ =>
              amend(base)(draw(None, Map.empty)(using form), deleteBtn)
            }
            val tpl =
              amend(base)(draw(None, Map.empty)(using form), deleteBtn)
            listOfSubformsContainer(
              label,
              names(idx),
              existing,
              tpl
            )
      }.toSeq*
    )
  }

  def validate[T[F[_]] <: Product](
      formData: T[[T] =>> T]
  )(using
      schema: T[FieldSchema]
  ): ZIO[Any, Nothing, Map[String, Seq[String]]] = {
    val schemas = schema.productIterator.toSeq
    val names = formData.productElementNames.toSeq
    ZIO
      .collectAllPar(formData.productIterator.zipWithIndex.map { (value, idx) =>
        type Gen
        val schema = schemas(idx).asInstanceOf[FieldSchema[Gen]]
        schema.validator.validate(value.asInstanceOf[Gen]).map { errors =>
          (names(idx), errors)
        }
      }.toSeq)
      .map(_.filter(_._2.nonEmpty).toMap)
  }

  def decodeAndValidate[T[F[_]] <: Product](
      input: zio.http.Form
  )(using
      schema: T[FieldSchema],
      decoder: FormDecoder[T[[T] =>> T]]
  ): ZIO[Any, IncompleteForm[T[[T] =>> T]], T[[T] =>> T]] =
    decoder.decode(input) match {
      case Left(errors) =>
        ZIO.fail(IncompleteForm(errors.groupMap(_.field)(_.message), None))
      case Right(decoded) =>
        validate(decoded).flatMap { validationErrors =>
          if (validationErrors.nonEmpty)
            ZIO.fail(IncompleteForm(validationErrors, Some(decoded)))
          else ZIO.succeed(decoded)
        }
    }
}
