package form4s

import zio.ZIO
import scala.deriving.Mirror
import scala.compiletime.summonInline

trait FormV2[Elem] {
  def base: Elem
  def amend(p: Elem)(inside: Elem*): Elem
  def render(in: Elem): String
  def subFormContainer(label: String): Elem
  def addBtn: Elem
  def deleteBtn: Elem
  def listOfSubformsContainer(
      label: String,
      fieldName: Cursor,
      items: Seq[Elem],
      templateItem: Elem
  ): Elem

  trait Renderable[T] { that =>
    def draw(
        schema: FieldSchema[T],
        fieldName: Cursor,
        oldValue: Option[T],
        errors: Seq[String]
    ): Elem

    def optional: Renderable[Option[T]] = new Renderable[Option[T]] {
      def draw(
          schema: FieldSchema[Option[T]],
          fieldName: Cursor,
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

  def draw[T[F[_]] <: Product](
      oldValue: Option[T[[T] =>> T]],
      errors: Map[String, Seq[String]]
  )(using
      schema: T[FormSchema]
  ): Elem = {
    draw(oldValue, errors, Cursor.root)
  }

  private def drawUnsafe[T[F[_]] <: Product](
      oldValue: Option[Any],
      errors: Map[String, Seq[String]],
      cursor: Cursor
  )(using
      schema: T[FormSchema]
  ): Elem =
    draw[T](oldValue.map(_.asInstanceOf[T[[T] =>> T]]), errors, cursor)(using
      schema
    )

  private def draw[T[F[_]] <: Product](
      oldValue: Option[T[[T] =>> T]],
      errors: Map[String, Seq[String]],
      cursor: Cursor
  )(using
      schema: T[FormSchema]
  ): Elem = {
    val oldValues = oldValue.map(_.productIterator.toSeq)
    val names = schema.productElementNames.toSeq
    amend(base)(
      schema.productIterator.zipWithIndex.map { (schemaAny, idx) =>
        type Gen
        val schema = schemaAny.asInstanceOf[FormSchema[Gen]]
        val name = names(idx)
        val subCursor = cursor.down(name)
        schema match
          case FormSchema.Field(schema) =>
            schema.renderer.draw(
              schema,
              subCursor,
              oldValues.map(v => v(idx).asInstanceOf[Gen]),
              errors.get(subCursor.build).getOrElse(Seq.empty)
            )
          case FormSchema.SubForm(label, form) =>
            amend(subFormContainer(label))(
              drawUnsafe(
                oldValues.map(v => v(idx)),
                errors,
                subCursor
              )(using form)
            )
          case FormSchema.RepeatedSubForm(label, form) =>
            val oldValuesForSeq =
              oldValues.map(v => v(idx).asInstanceOf[Seq[Any]])
            val count =
              oldValuesForSeq.map(_.length).getOrElse(0)
            val existing = (0 until count).map { i =>
              amend(base)(
                drawUnsafe(
                  oldValuesForSeq.map(v => v(i)),
                  errors,
                  subCursor.at(i)
                )(using form),
                deleteBtn
              )
            }
            val tpl =
              amend(base)(
                draw(None, Map.empty, subCursor.at(0))(using form),
                deleteBtn
              )
            listOfSubformsContainer(
              label,
              subCursor,
              existing,
              tpl
            )
      }.toSeq*
    )
  }

  private def validateUnsafe[T[F[_]] <: Product](
      data: Any,
      schema: T[FormSchema],
      cursor: Cursor
  ) =
    validate(data.asInstanceOf[T[[T] =>> T]], cursor)(using schema)

  private def validate[T[F[_]] <: Product](
      formData: T[[T] =>> T],
      cursor: Cursor
  )(using
      schema: T[FormSchema]
  ): ZIO[Any, Nothing, Map[Cursor, Seq[String]]] = {
    val schemas = schema.productIterator.toSeq
    val names = formData.productElementNames.toSeq
    ZIO
      .collectAllPar(formData.productIterator.zipWithIndex.map { (value, idx) =>
        type Gen
        val schema = schemas(idx).asInstanceOf[FormSchema[Gen]]
        val name = names(idx)
        schema match
          case FormSchema.Field(schema) =>
            schema.validator.validate(value.asInstanceOf[Gen]).map { errors =>
              Seq((cursor.down(name), errors)).toMap
            }
          case FormSchema.SubForm(label, schema) =>
            validateUnsafe(value, schema, cursor.down(name))
          case FormSchema.RepeatedSubForm(label, schema) =>
            val subCursor = cursor.down(name)
            ZIO
              .collectAllPar(value.asInstanceOf[Seq[Any]].zipWithIndex.map {
                (v, i) =>
                  validateUnsafe(v, schema, subCursor.at(i))
              })
              .map(_.foldLeft(Map.empty[Cursor, Seq[String]])(_ ++ _))
      }.toSeq)
      .map(
        _.foldLeft(Map.empty[Cursor, Seq[String]])(_ ++ _)
          .filter((_, error) => error.nonEmpty)
      )
  }

  def validate[T[F[_]] <: Product](
      formData: T[[T] =>> T]
  )(using
      schema: T[FormSchema]
  ): ZIO[Any, Nothing, Map[String, Seq[String]]] =
    validate(formData, Cursor.root)(using schema)
      .map(_.map((c, e) => (c.build, e)))

  def decodeAndValidate[T[F[_]] <: Product](
      input: zio.http.Form
  )(using
      schema: T[FormSchema],
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
