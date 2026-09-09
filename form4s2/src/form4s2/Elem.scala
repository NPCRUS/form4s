package form4s2

/** Minimal, format-agnostic render output. Users build `Elem` values in their
  * field render functions; a concrete renderer (e.g. HTML) can walk this tree.
  */
enum Elem:
  case Text(value: String)
  case Node(
      tag: String,
      attrs: List[(String, String)] = Nil,
      children: List[Elem] = Nil
  )
  case Fragment(children: List[Elem])

object Elem:
  val empty: Elem = Fragment(Nil)

  def fragment(children: List[Elem]): Elem = Fragment(children)

  def node(tag: String, attrs: List[(String, String)] = Nil)(
      children: Elem*
  ): Elem = Node(tag, attrs, children.toList)

  def text(value: String): Elem = Text(value)

  /** Render to a simple HTML string. */
  def render(e: Elem): String = e match
    case Text(value)       => value
    case Fragment(cs)      => cs.map(render).mkString
    case Node(tag, as, cs) =>
      val attrStr =
        if as.isEmpty then ""
        else as.map((k, v) => s"""$k="$v"""").mkString(" ", " ", "")
      if cs.isEmpty then s"<$tag$attrStr></$tag>"
      else s"<$tag$attrStr>${cs.map(render).mkString}</$tag>"
