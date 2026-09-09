package form4s2

import zio.blocks.schema.{CompanionOptics, Lens}

/** A typed access path from `S` to `A`, carrying a runtime field name (used for
  * HTML `name` attributes and error map keys).
  */
trait Field[S, A]:
  def name: String
  def get(s: S): A

/** A leaf field of a record `S`, tagged with the compile-time literal field
  * name `N`. `N` is used by the totality check in [[form4s2.draw]].
  */
final class LeafField[S, A, N <: String](
    val lens: Lens[S, A],
    val name: String
) extends Field[S, A]:
  def get(s: S): A = lens.get(s)
  def replace(s: S, a: A): S = lens.replace(s, a)

  /** Compose with another field, yielding a plain [[Path]] (name and getter).
    */
  def /[B](that: Field[A, B]): Path[S, B] =
    Path(s"$name.${that.name}", s => that.get(lens.get(s)))

object LeafField:
  def of[S, A](lens: Lens[S, A], name: String): LeafField[S, A, Nothing] =
    new LeafField[S, A, Nothing](lens, name)

/** A composite access path: a dot-notation name plus a getter. */
final class Path[S, A](val name: String, f: S => A) extends Field[S, A]:
  def get(s: S): A = f(s)

object Path:
  def apply[S, A](name: String, f: S => A): Path[S, A] = new Path(name, f)

/** Mixin for form model companions. Provides `field(_.name)` producing a
  * name-tagged [[LeafField]] whose compile-time name is derived from the
  * selector, never hardcoded.
  */
trait FormFields[S] extends CompanionOptics[S]:
  transparent inline def field[A](inline sel: S => A): LeafField[S, A, ?] =
    ${ form4s2.FieldMacro.fieldImpl[S, A]('sel) }
