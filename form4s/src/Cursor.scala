package form4s

/** Accumulates dot-notation path segments during form rendering and validation.
  * Produces field names (`address.city`) and error map keys matching the naming
  * convention used by [[FormDecoder]].
  *
  * The cursor is built by pushing segments onto a list (rightmost = deepest)
  * and rendered in reverse on [[build]].
  *
  * @param segments
  *   path segments, deepest first
  */
case class Cursor(segments: List[String]) {

  /** Push a named field segment, e.g. `"city"` → `address.city`. */
  def down(name: String): Cursor = Cursor(name :: segments)

  /** Push an indexed field segment, e.g. `0` → `docs.0.typ`. */
  def at(index: Int): Cursor = Cursor(index.toString :: segments)

  /** Render the accumulated path in dot notation, e.g. `"address.city"`. */
  def build: String = segments.reverse.mkString(".")
}

object Cursor {
  val root: Cursor = Cursor(Nil)
}
