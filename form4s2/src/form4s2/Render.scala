package form4s2

import scala.deriving.Mirror

/** Ambient, user-extensible render context. Subclass to add e.g. auth, locale,
  * or feature flags.
  */
trait RenderEnv:
  def mode: RenderEnv.Mode = RenderEnv.Mode.Edit

object RenderEnv:
  enum Mode:
    case Edit, View

  val empty: RenderEnv = new RenderEnv {}
  val view: RenderEnv = new RenderEnv { override def mode = Mode.View }

/** Context for the node currently being drawn: its dot-notation path, prior
  * value, error map, and the ambient [[RenderEnv]].
  */
final case class RenderContext[A](
    path: String,
    oldValue: Option[A],
    errors: Map[String, Seq[String]],
    env: RenderEnv
):
  /** Errors for this exact node. */
  def error: Seq[String] = errors.getOrElse(path, Seq.empty)

  /** Dot-notation path of a named child. */
  def child(name: String): String =
    if path.isEmpty then name else s"$path.$name"

/** A single drawn field of a record `S`, tagged with the compile-time literal
  * field name `N`.
  */
final class Drawn[S, N <: String](
    val name: String,
    val step: RenderContext[S] => Elem
):
  def render(ctx: RenderContext[S]): Elem = step(ctx)

  /** Conditionally render this field based on its context. */
  def showIf(p: RenderContext[S] => Boolean): Drawn[S, N] =
    new Drawn[S, N](name, ctx => if p(ctx) then step(ctx) else Elem.empty)

  /** Never render this field (still declared for totality). */
  def hide: Drawn[S, N] = new Drawn[S, N](name, _ => Elem.empty)

/** A rendered form for a record `S`: a function from context to `Elem`. */
final class FormRenderer[S](val render: RenderContext[S] => Elem):
  def apply(ctx: RenderContext[S]): Elem = render(ctx)

object FormRenderer:
  def apply[S](f: RenderContext[S] => Elem): FormRenderer[S] = new FormRenderer(
    f
  )

/** Draw a single leaf field. The field name comes from the lens (never
  * hardcoded); the render function receives a [[RenderContext]] carrying the
  * path, old value, and errors.
  */
inline def drawField[S, A, N <: String](f: LeafField[S, A, N])(
    render: RenderContext[A] => Elem
): Drawn[S, N] =
  new Drawn[S, N](
    f.name,
    (ctx: RenderContext[S]) =>
      render(
        RenderContext(
          ctx.child(f.name),
          ctx.oldValue.map(f.get),
          ctx.errors,
          ctx.env
        )
      )
  )

/** Draw a nested sub-form (a record field). The inner form is checked for
  * totality independently.
  */
inline def subForm[S, A, N <: String](
    f: LeafField[S, A, N],
    label: String
)(inner: FormRenderer[A]): Drawn[S, N] =
  new Drawn[S, N](
    f.name,
    (ctx: RenderContext[S]) =>
      val childPath = ctx.child(f.name)
      val childCtx =
        RenderContext(childPath, ctx.oldValue.map(f.get), ctx.errors, ctx.env)
      val errs = childCtx.error
        .map(e => Elem.node("span", List("class" -> "error"))(Elem.text(e)))
        .toList
      Elem.Node(
        "fieldset",
        Nil,
        Elem.node("legend")(Elem.text(label)) :: errs ::: List(
          inner.render(childCtx)
        )
      )
  )

/** Draw a repeated sub-form (a `Seq` field): one item per existing element plus
  * a hidden template for client-side add.
  */
inline def repeated[S, A, N <: String](
    f: LeafField[S, Seq[A], N],
    label: String,
    required: Boolean = false
)(inner: FormRenderer[A]): Drawn[S, N] =
  new Drawn[S, N](
    f.name,
    (ctx: RenderContext[S]) =>
      val basePath = ctx.child(f.name)
      val items = ctx.oldValue.map(f.get).getOrElse(Seq.empty)
      val sectionErrors = ctx.errors.getOrElse(basePath, Seq.empty)

      val rendered = items.zipWithIndex.map { (item, i) =>
        val childCtx =
          RenderContext(s"$basePath.$i", Some(item), ctx.errors, ctx.env)
        Elem.node("div", List("data-item" -> ""))(inner.render(childCtx))
      }.toList

      val tplCtx =
        RenderContext(s"$basePath.0", Option.empty[A], Map.empty, ctx.env)
      val template = Elem.node("template")(inner.render(tplCtx))

      val errs = sectionErrors
        .map(e => Elem.node("span", List("class" -> "error"))(Elem.text(e)))
        .toList

      Elem.Node(
        "fieldset",
        Nil,
        Elem.node("legend")(Elem.text(label)) :: errs ::: rendered ::: List(
          template,
          Elem.node("button", List("type" -> "button"))(Elem.text("Add"))
        )
      )
  )

/** Combine drawn fields into a complete form. The tuple of [[Drawn]] values is
  * checked at compile time to cover every field of the record exactly once.
  */
inline def draw[T <: Tuple](fields: T)(using
    m: Mirror.ProductOf[Src[T]]
): FormRenderer[Src[T]] =
  checkTotality[m.MirroredElemLabels, Names[T]]
  val steps: List[RenderContext[Src[T]] => Elem] =
    fields.toList.map(f => f.asInstanceOf[Drawn[Src[T], ?]].step)
  FormRenderer[Src[T]](ctx => Elem.fragment(steps.map(_(ctx))))

/** Render a form at the top level, supplying old values and errors. */
def renderForm[S](
    form: FormRenderer[S],
    oldValue: Option[S] = None,
    errors: Map[String, Seq[String]] = Map.empty,
    env: RenderEnv = RenderEnv.empty
): Elem =
  form.render(RenderContext("", oldValue, errors, env))
