package form4s

import zio.blocks.schema.Schema

import scala.language.dynamics

/** Runtime holder for auto-derived fields. Static field types come from the
  * refined structural type produced by [[AutoFieldsMacro]]; at runtime each
  * access goes through `selectDynamic`.
  */
final class FieldsHolder(members: Map[String, Any])
    extends scala.Dynamic
    with Selectable:
  def selectDynamic(name: String): Any =
    members.getOrElse(name, sys.error(s"No field '$name'"))

/** Mixin for form model companions that auto-derives a name-tagged
  * [[LeafField]] for every field of `S`, so there is no need to declare
  * `val x = field(_.x)` manually.
  *
  * Example:
  * {{{
  * case class Registration(username: String, email: String)
  * object Registration extends AutoFields[Registration]:
  *   given Schema[Registration] = Schema.derived
  *
  * val u = Registration.username // LeafField[Registration, String, "username"]
  * }}}
  *
  * A user-defined member on the companion (e.g. a manual `field(_.x)`) always
  * takes precedence over the derived one.
  *
  * @see
  *   [[FormFields]] for the explicit `field(_.x)` alternative.
  */
trait AutoFields[S] extends FormFields[S]:
  given toFields(using
      fieldsFor: FieldsFor[S]
  ): Conversion[this.type, fieldsFor.Out] =
    _ => fieldsFor.fields

/** Typeclass whose `Out` is the refined structural type carrying one
  * `LeafField[S, A, N]` member per field of `S`. Usually summoned automatically
  * by the `AutoFields` conversion rather than used directly.
  */
trait FieldsFor[S]:
  type Out
  def fields: Out

object FieldsFor:
  transparent inline given derived[S](using schema: Schema[S]): FieldsFor[S] =
    ${ form4s.AutoFieldsMacro.fieldsForImpl[S]('schema) }
