package form4s

import scala.deriving.Mirror

/** Context for the node currently being drawn: its dot-notation path, prior
  * value, and error map.
  */
final case class RenderContext[A](
    path: String,
    oldValue: Option[A],
    errors: Map[String, Seq[String]]
):
  /** Errors for this exact node. */
  def error: Seq[String] = errors.getOrElse(path, Seq.empty)

  /** Dot-notation path of a named child. */
  def child(name: String): String =
    if path.isEmpty then name else s"$path.$name"

  /** Indexed path, for repeated sub-form items. */
  def at(index: Int): String = s"$path.$index"

/** A single drawn field of a record `S`, tagged with the compile-time literal
  * field name `N` and the output element type `E`.
  */
final class Drawn[S, N <: String, E](
    val name: String,
    val step: RenderContext[S] => E
):
  def render(ctx: RenderContext[S]): E = step(ctx)

/** A rendered form for a record `S`: a function from context to `E`. */
final class FormRenderer[S, E](val render: RenderContext[S] => E):
  def apply(ctx: RenderContext[S]): E = render(ctx)

object FormRenderer:
  def apply[S, E](f: RenderContext[S] => E): FormRenderer[S, E] =
    new FormRenderer(
      f
    )

/** Draw a single leaf field. The field name comes from the lens (never
  * hardcoded); the render function receives a [[RenderContext]] carrying the
  * path, old value, and errors.
  */
inline def drawField[S, A, N <: String, E](f: LeafField[S, A, N])(
    render: RenderContext[A] => E
): Drawn[S, N, E] =
  new Drawn[S, N, E](
    f.name,
    (ctx: RenderContext[S]) =>
      render(
        RenderContext(
          ctx.child(f.name),
          ctx.oldValue.map(f.get),
          ctx.errors
        )
      )
  )

/** Combine drawn fields into a complete form. The tuple of [[Drawn]] values is
  * checked at compile time to cover every field of the record exactly once, and
  * their rendered output is folded together with the user-supplied `combine`.
  */
inline def draw[T <: Tuple](fields: T)(combine: (Out[T], Out[T]) => Out[T])(
    using m: Mirror.ProductOf[Src[T]]
): FormRenderer[Src[T], Out[T]] =
  checkTotality[m.MirroredElemLabels, Names[T]]
  val steps: List[RenderContext[Src[T]] => Out[T]] =
    fields.toList.map(f => f.asInstanceOf[Drawn[Src[T], ?, Out[T]]].step)
  FormRenderer[Src[T], Out[T]](ctx => steps.map(_(ctx)).reduce(combine))

/** Render a form at the top level, supplying old values and errors. */
def renderForm[S, E](
    form: FormRenderer[S, E],
    oldValue: Option[S] = None,
    errors: Map[String, Seq[String]] = Map.empty
): E =
  form.render(RenderContext("", oldValue, errors))
