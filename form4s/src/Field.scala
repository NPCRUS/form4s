package form4s

import zio.blocks.schema.{CompanionOptics, Lens}

/** A typed access path from a source `S` to a value `A`, carrying a runtime
  * field name (used for HTML `name` attributes and error-map keys).
  *
  * Fields are the single source of field identity shared by rendering, decoding
  * and validation. They are obtained via [[FormFields.field]] or
  * [[AutoFields]]; see [[LeafField]] for the concrete type and [[Path]] for
  * composed paths.
  */
trait Field[S, A]:
  /** The dot-notation name of this field (e.g. `"city"` or `"address.city"`).
    */
  def name: String

  /** Read the focused value out of `s`. */
  def get(s: S): A

/** A leaf field of a record `S`, tagged with the compile-time literal field
  * name `N`. `N` drives the [[draw]] totality check and is derived from the
  * case-class field name by the compiler — never hardcoded as a string.
  *
  * Backed by a `zio.blocks.schema.Lens` for `get`/`replace`.
  */
final class LeafField[S, A, N <: String](
    val lens: Lens[S, A],
    val name: String
) extends Field[S, A]:
  def get(s: S): A = lens.get(s)

  /** Immutably set the focused field to `a`. */
  def replace(s: S, a: A): S = lens.replace(s, a)

  /** Compose with another field, yielding a plain [[Path]] (name + getter) for
    * nested access, e.g. `user.address / Address.city` → `"address.city"`.
    */
  def /[B](that: Field[A, B]): Path[S, B] =
    Path(s"$name.${that.name}", s => that.get(lens.get(s)))

object LeafField:
  /** Internal constructor used by the `field`/`AutoFields` macros. */
  def of[S, A](lens: Lens[S, A], name: String): LeafField[S, A, Nothing] =
    new LeafField[S, A, Nothing](lens, name)

/** A composite access path: a dot-notation name plus a getter. Produced by
  * composing two [[LeafField]]s with `/`.
  */
final class Path[S, A](val name: String, f: S => A) extends Field[S, A]:
  def get(s: S): A = f(s)

object Path:
  def apply[S, A](name: String, f: S => A): Path[S, A] = new Path(name, f)

/** Mixin for form model companions. Provides `field(_.name)`, producing a
  * name-tagged [[LeafField]] whose compile-time name is derived from the
  * selector, never hardcoded.
  *
  * Prefer [[AutoFields]] when you want every field derived without writing
  * `val`s.
  */
trait FormFields[S] extends CompanionOptics[S]:
  /** Derive a leaf field from a field selector. The name is taken from the
    * selector by a macro, so it cannot drift from the case class.
    *
    * {{{
    * import zio.blocks.schema.Schema
    *
    * case class User(name: String, age: Int)
    * object User extends FormFields[User]:
    *   given Schema[User] = Schema.derived
    *   val name = field(_.name) // LeafField[User, String, "name"]
    *   val age = field(_.age)   // LeafField[User, Int, "age"]
    * }}}
    */
  transparent inline def field[A](inline sel: S => A): LeafField[S, A, ?] =
    ${ form4s.FieldMacro.fieldImpl[S, A]('sel) }
