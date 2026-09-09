package form4s2

import zio.http.template2.{Dom, Modifier}

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
    val step: RenderContext[S] => Dom
):
  def render(ctx: RenderContext[S]): Dom = step(ctx)

  /** Conditionally render this field based on its context. */
  def showIf(p: RenderContext[S] => Boolean): Drawn[S, N] =
    new Drawn[S, N](name, ctx => if p(ctx) then step(ctx) else Dom.empty)

  /** Never render this field (still declared for totality). */
  def hide: Drawn[S, N] = new Drawn[S, N](name, _ => Dom.empty)

/** A rendered form for a record `S`: a function from context to `Dom`. */
final class FormRenderer[S](val render: RenderContext[S] => Dom):
  def apply(ctx: RenderContext[S]): Dom = render(ctx)

object FormRenderer:
  def apply[S](f: RenderContext[S] => Dom): FormRenderer[S] = new FormRenderer(
    f
  )

/** Draw a single leaf field. The field name comes from the lens (never
  * hardcoded); the render function receives a [[RenderContext]] carrying the
  * path, old value, and errors.
  */
inline def drawField[S, A, N <: String](f: LeafField[S, A, N])(
    render: RenderContext[A] => Dom
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
      val errs = childCtx.error.map(err)
      el("fieldset")(
        (Seq(el("legend")(Dom.text(label)), inner.render(childCtx)) ++ errs)*
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
        el("div", List("data-item" -> ""))(inner.render(childCtx))
      }

      val tplCtx =
        RenderContext(s"$basePath.0", Option.empty[A], Map.empty, ctx.env)
      val template = el("template")(inner.render(tplCtx))

      el("fieldset")(
        (
          Seq(el("legend")(Dom.text(label))) ++
            sectionErrors.map(err) ++
            rendered ++
            Seq(
              template,
              el("button", List("type" -> "button"))(Dom.text("Add"))
            )
        )*
      )
  )

/** Combine drawn fields into a complete form. The tuple of [[Drawn]] values is
  * checked at compile time to cover every field of the record exactly once.
  */
inline def draw[T <: Tuple](fields: T)(using
    m: Mirror.ProductOf[Src[T]]
): FormRenderer[Src[T]] =
  checkTotality[m.MirroredElemLabels, Names[T]]
  val steps: List[RenderContext[Src[T]] => Dom] =
    fields.toList.map(f => f.asInstanceOf[Drawn[Src[T], ?]].step)
  FormRenderer[Src[T]](ctx => Dom.fragment(steps.map(_(ctx))*))

/** Render a form at the top level, supplying old values and errors. */
def renderForm[S](
    form: FormRenderer[S],
    oldValue: Option[S] = None,
    errors: Map[String, Seq[String]] = Map.empty,
    env: RenderEnv = RenderEnv.empty
): Dom =
  form.render(RenderContext("", oldValue, errors, env))

private def el(tag: String, attrs: List[(String, String)] = Nil)(
    children: Dom*
): Dom =
  val modifiers: List[Modifier] =
    attrs.map((k, v) => Dom.attr(k, v): Modifier) ++ children
  Dom.element(tag).apply(modifiers)

private def err(message: String): Dom =
  el("span", List("class" -> "error"))(Dom.text(message))
