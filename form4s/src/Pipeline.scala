package form4s

import zio.*
import zio.http.Form

/** Error value returned by [[decodeAndValidate]] on failure, for re-rendering.
  */
final case class Incomplete[A](
    errors: Map[String, Seq[String]],
    oldForm: Option[A]
)

/** Decode then validate in one step. Fails with an [[Incomplete]] containing
  * all errors and (on validation failure) the partially decoded form for
  * re-rendering.
  */
def decodeAndValidate[A](form: Form)(using
    FormDecoder[A],
    Validation[A]
): ZIO[Any, Incomplete[A], A] =
  summon[FormDecoder[A]].decode(form) match
    case Left(errs) =>
      ZIO.fail(Incomplete(errs.groupMap(_.field)(_.message), None))
    case Right(a) =>
      summon[Validation[A]].run(a).flatMap { errs =>
        if errs.nonEmpty then ZIO.fail(Incomplete(errs, Some(a)))
        else ZIO.succeed(a)
      }
