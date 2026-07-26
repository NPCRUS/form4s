package form4s

case class Cursor(segments: List[String]) {
  def down(name: String): Cursor = Cursor(name :: segments)
  def at(index: Int): Cursor = Cursor(index.toString :: segments)
  def build: String = segments.reverse.mkString(".")
}
object Cursor {
  val root: Cursor = Cursor(Nil)
}
