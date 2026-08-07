package form4s

import zio.ZIO
import scala.deriving.Mirror
import scala.compiletime.summonInline
import scala.compiletime.erasedValue

/** Determines at compile time whether a field of type `T` should be marked
  * required by default. `Option` fields are non-required; all others are
  * required.
  */
inline def defaultRequired[T]: Boolean = inline erasedValue[T] match {
  case _: Option[?] => false
  case _            => true
}

/** Rendering algebra parameterized by an element type `Elem`. Implement this
  * trait to define how fields, sub-forms, and buttons are rendered for a
  * specific output format (e.g. HTML, terminal, JSON).
  *
  * Forms use a higher-kinded encoding: user models are
  * `case class X[F[_]](...)` where raw data is `X[[T] =>> T]]` and schema is
  * `X[FormSchema]`.
  *
  * The main entry points are:
  *   - [[draw]] — render the form with old values and errors
  *   - [[validate]] — run field-level validators
  *   - [[decodeAndValidate]] — decode form input + validate, returning either
  *     the typed data or an [[IncompleteForm]] for re-rendering
  *
  * @tparam Elem
  *   the output element type (e.g. `Dom` for HTML, `String` for text)
  */
trait Form[Elem] {

  /** The root container element for the entire form. */
  def base: Elem

  /** Amend `p` with child elements `inside` (e.g. nest children within a
    * container).
    */
  def amend(p: Elem)(inside: Elem*): Elem

  /** Render an element to a string representation. */
  def render(in: Elem): String

  /** Render a container for a sub-form section, including any section-level
    * error messages.
    */
  def subFormContainer(label: String, errors: Seq[String]): Elem

  /** Render an "add item" button for repeated sub-forms. */
  def addBtn: Elem

  /** Render a "delete item" button for repeated sub-form entries. */
  def deleteBtn: Elem

  /** Render a container for repeated sub-forms. Includes a label, existing
    * items, a hidden template item for client-side cloning, and section-level
    * error messages.
    *
    * @param fieldName
    *   dot-notation path to this repeated form section
    * @param items
    *   rendered existing entries
    * @param templateItem
    *   rendered template for new entries
    * @param required
    *   whether at least one entry is required
    */
  def listOfSubformsContainer(
      label: String,
      fieldName: Cursor,
      items: Seq[Elem],
      templateItem: Elem,
      errors: Seq[String],
      required: Boolean
  ): Elem

  /** Typeclass for rendering a single form field of type `T`. Implement this to
    * define how a field type (e.g. text input, checkbox, date picker) is drawn.
    *
    * @tparam T
    *   the value type of the field
    */
  trait Renderable[T] { that =>

    /** Draw a form field given its schema, dot-notation path, previous value,
      * and current validation errors.
      */
    def draw(
        schema: FieldSchema[T],
        fieldName: Cursor,
        oldValue: Option[T],
        errors: Seq[String]
    ): Elem

    /** Lift this renderer to handle [[Option]] fields: renders the underlying
      * type with `required = false`.
      */
    def optional: Renderable[Option[T]] = new Renderable[Option[T]] {
      def draw(
          schema: FieldSchema[Option[T]],
          fieldName: Cursor,
          oldValue: Option[Option[T]],
          errors: Seq[String]
      ): Elem =
        that.draw(
          FieldSchema[T](
            label = schema.label,
            renderer = that,
            placeholderAttr = schema.placeholderAttr,
            typeAttr = schema.typeAttr,
            required = false
          ),
          fieldName,
          oldValue.flatten,
          errors
        )
    }
  }

  /** Schema for a single form element within a higher-kinded form model.
    * Describes whether the element is a field, a nested sub-form, or a repeated
    * sub-form.
    *
    * @tparam T
    *   the value type this schema describes
    */
  enum FormSchema[T] {

    /** A single form field, e.g. a text input or checkbox. */
    case Field[T](schema: FieldSchema[T]) extends FormSchema[T]

    /** A nested sub-form, e.g. an address block inside a user form.
      *
      * @param validator
      *   optional section-level validator (e.g. check that all fields are
      *   either filled or empty)
      */
    case SubForm[F[B[_]] <: Product](
        label: String,
        form: F[FormSchema],
        validator: ValidatorZIO[F[[T] =>> T]] = ValidatorZIO.empty[F[[T] =>> T]]
    ) extends FormSchema[F[FormSchema]]

    /** A repeated sub-form, e.g. a list of document entries. Supports
      * add/delete via [[addBtn]]/[[deleteBtn]].
      *
      * @param required
      *   whether at least one entry is mandatory
      * @param validator
      *   optional section-level validator
      */
    case RepeatedSubForm[F[B[_]] <: Product](
        label: String,
        form: F[FormSchema],
        required: Boolean = false,
        validator: ValidatorZIO[Seq[F[[T] =>> T]]] =
          ValidatorZIO.empty[Seq[F[[T] =>> T]]]
    ) extends FormSchema[Seq[F[FormSchema]]]
  }

  /** Configuration for a single form field: its label, renderer, HTML
    * attributes, validator, and required state.
    *
    * @param label
    *   human-readable field label
    * @param renderer
    *   how to draw this field for the target `Elem`
    * @param placeholderAttr
    *   HTML placeholder attribute value (empty = none)
    * @param typeAttr
    *   HTML `type` attribute value (e.g. `"text"`, `"checkbox"`)
    * @param validator
    *   effectful validator; defaults to [[Validator.empty]]
    * @param required
    *   whether this field must be non-empty; defaults to [[defaultRequired]]
    */
  case class FieldSchema[T](
      label: String,
      renderer: Renderable[T],
      placeholderAttr: String,
      typeAttr: String,
      validator: ValidatorZIO[T] = Validator.empty[T].toZIO,
      required: Boolean = defaultRequired[T]
  )

  /** Render the full form. Populates fields with old values (for re-display
    * after submission) and displays validation errors next to the relevant
    * fields.
    *
    * @param oldValue
    *   previously decoded form data, if available (e.g. after validation
    *   failure)
    * @param errors
    *   validation errors keyed by dot-notation field path
    * @param schema
    *   the form schema describing fields and sub-forms
    * @return
    *   the rendered `Elem`
    */
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
          case FormSchema.SubForm(label, form, _) =>
            amend(
              subFormContainer(
                label,
                errors.get(subCursor.build).getOrElse(Seq.empty)
              )
            )(
              drawUnsafe(
                oldValues.map(v => v(idx)),
                errors,
                subCursor
              )(using form)
            )
          case FormSchema.RepeatedSubForm(label, form, required, _) =>
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
              tpl,
              errors.get(subCursor.build).getOrElse(Seq.empty),
              required
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

  private def validateUnsafe[T[F[_]]](
      validator: ValidatorZIO[T[[T] =>> T]],
      value: Any
  ) = validator.validate(value.asInstanceOf[T[[T] =>> T]])

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
            val requiredValidator: Validator[Gen] =
              if (!schema.required) Validator.empty
              else
                value.asInstanceOf[Gen] match {
                  case _: String =>
                    Validator.nonEmpty.asInstanceOf[Validator[Gen]]
                  case _: Option[?] =>
                    Validator.required.asInstanceOf[Validator[Gen]]
                  case _ => Validator.empty
                }
            ValidatorZIO
              .compose(schema.validator, requiredValidator.toZIO)
              .validate(value.asInstanceOf[Gen])
              .map { errors =>
                Seq((cursor.down(name), errors)).toMap
              }
          case FormSchema.SubForm(label, schema, validator) =>
            for {
              subErrors <- validateUnsafe(value, schema, cursor.down(name))
              wholeErrors <- validateUnsafe(validator, value)
                .map { errs =>
                  if (errs.nonEmpty) Map(cursor.down(name) -> errs)
                  else Map.empty[Cursor, Seq[String]]
                }
            } yield subErrors ++ wholeErrors
          case FormSchema.RepeatedSubForm(label, schema, required, validator) =>
            val subCursor = cursor.down(name)
            val requiredValidator: Validator[Seq[?]] =
              if (required) Validator.nonEmptySeq else Validator.empty
            for {
              subErrors <- ZIO
                .collectAllPar(value.asInstanceOf[Seq[Any]].zipWithIndex.map {
                  (v, i) =>
                    validateUnsafe(v, schema, subCursor.at(i))
                })
                .map(_.foldLeft(Map.empty[Cursor, Seq[String]])(_ ++ _))
              wholeErrors <- ValidatorZIO
                .compose(
                  validator.asInstanceOf[ValidatorZIO[Seq[?]]],
                  requiredValidator.toZIO
                )
                .validate(value.asInstanceOf[Seq[?]])
                .map { errs =>
                  if (errs.nonEmpty) Map(subCursor -> errs)
                  else Map.empty[Cursor, Seq[String]]
                }
            } yield subErrors ++ wholeErrors
      }.toSeq)
      .map(
        _.foldLeft(Map.empty[Cursor, Seq[String]])(_ ++ _)
          .filter((_, error) => error.nonEmpty)
      )
  }

  /** Run all field and section validators against decoded form data. Returns a
    * map from dot-notation field names to error messages. Fields with no errors
    * are excluded from the result.
    *
    * @param formData
    *   decoded form values
    * @param schema
    *   the form schema
    * @return
    *   a map of field paths to error messages (empty map = all valid)
    */
  def validate[T[F[_]] <: Product](
      formData: T[[T] =>> T]
  )(using
      schema: T[FormSchema]
  ): ZIO[Any, Nothing, Map[String, Seq[String]]] =
    validate(formData, Cursor.root)(using schema)
      .map(_.map((c, e) => (c.build, e)))

  /** Decode raw form input and validate in one step. On success, returns the
    * typed form data. On failure, returns an [[IncompleteForm]] containing all
    * errors and (on validation failure) the partially decoded form for
    * re-rendering.
    *
    * @param input
    *   raw `zio.http.Form` from the HTTP request
    * @param schema
    *   the form schema
    * @param decoder
    *   the [[FormDecoder]] instance (auto-derived via Magnolia)
    * @return
    *   `ZIO.fail(IncompleteForm(...))` if decode or validation fails;
    *   `ZIO.succeed(decoded)` if all is well
    */
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

/** Error value returned by [[Form.decodeAndValidate]] when decoding or
  * validation fails. Carry it through to re-render the form via [[Form.draw]].
  *
  * @param errors
  *   validation/decode errors keyed by dot-notation field path
  * @param oldForm
  *   partially decoded form data (`None` on decode failure, `Some` on
  *   validation failure)
  * @tparam A
  *   the raw-data form type (e.g. `UserForm[[T] =>> T]`)
  */
case class IncompleteForm[A](
    errors: Map[String, Seq[String]],
    oldForm: Option[A]
)

object IncompleteForm {
  def empty[A]: IncompleteForm[A] = IncompleteForm(Map.empty, None)
}
