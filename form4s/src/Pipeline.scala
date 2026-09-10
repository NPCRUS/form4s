package form4s

import zio.*
import zio.http.Form

/** Error value returned by [[decodeAndValidate]] on failure, for re-rendering.
  *
  * @param errors
  *   decode/validation errors keyed by dot-notation field path.
  * @param oldForm
  *   partially decoded form data: `Some` on validation failure (so inputs can
  *   be re-filled), `None` on decode failure.
  */
final case class Incomplete[A](
    errors: Map[String, Seq[String]],
    oldForm: Option[A]
)

/** Carries the validation environment type `Env` for a form type `A`, so
  * [[decodeAndValidate]] can be invoked as `decodeAndValidate[A](form)` and
  * infer `Env` from the given [[Validation]]. Users do not implement this
  * directly; it is derived automatically from a `given Validation[A, R]`.
  */
trait ValidationRunner[A]:
  type Env
  def run(form: Form)(using FormDecoder[A]): ZIO[Env, Incomplete[A], A]

object ValidationRunner:
  type Aux[A, R] = ValidationRunner[A] { type Env = R }

  given derived[A, R](using v: Validation[A, R]): Aux[A, R] =
    new ValidationRunner[A]:
      type Env = R
      def run(form: Form)(using FormDecoder[A]): ZIO[R, Incomplete[A], A] =
        summon[FormDecoder[A]].decode(form) match
          case Left(errs) =>
            ZIO.fail(Incomplete(errs.groupMap(_.field)(_.message), None))
          case Right(a) =>
            v.run(a).flatMap { errs =>
              if errs.nonEmpty then ZIO.fail(Incomplete(errs, Some(a)))
              else ZIO.succeed(a)
            }

/** Decode then validate in one step, mirroring the classic form pipeline. Fails
  * with an [[Incomplete]] containing all errors and (on validation failure) the
  * partially decoded form for re-rendering via [[renderForm]].
  *
  * The validation environment `R` is inferred from the given [[Validation]], so
  * it is not spelled out at the call site.
  *
  * {{{
  * import zio.*
  * import zio.http.*
  * import zio.blocks.schema.Schema
  *
  * case class User(name: String)
  * object User extends AutoFields[User]:
  *   given Schema[User] = Schema.derived
  *
  * given FormDecoder[User] = FormDecoder.derived
  * given Validation[User, Any] = Validation(rule(User.name, Validator.nonEmpty))
  *
  * // a POST handler fragment:
  * def handle(fd: Form) =
  *   decodeAndValidate[User](fd).fold(
  *     incomplete => ZIO.succeed(Response.text("errors")),          // re-render
  *     user        => ZIO.succeed(Response.text(s"ok: ${user.name}"))
  *   )
  * }}}
  *
  * @see
  *   [[FormDecoder.derived]] and [[Validation]] provide the two typeclasses
  *   this relies on.
  */
def decodeAndValidate[A](form: Form)(using
    d: FormDecoder[A],
    v: ValidationRunner[A]
): ZIO[v.Env, Incomplete[A], A] =
  v.run(form)(using d)
