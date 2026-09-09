package form4s2

import zio.http.Form

/** Error value returned by [[decodeAndValidate]] on failure, for re-rendering.
  */
final case class Incomplete[A](
    errors: Map[String, Seq[String]],
    oldForm: Option[A]
)

/** Decode then validate in one step, mirroring the classic form pipeline. */
def decodeAndValidate[A](form: Form)(using
    FormDecoder[A],
    Validation[A]
): Either[Incomplete[A], A] =
  summon[FormDecoder[A]].decode(form) match
    case Left(errs) => Left(Incomplete(errs.groupMap(_.field)(_.message), None))
    case Right(a)   =>
      val errs = summon[Validation[A]].run(a)
      if errs.nonEmpty then Left(Incomplete(errs, Some(a)))
      else Right(a)
