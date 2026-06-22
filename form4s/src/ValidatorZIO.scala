package form4s

import zio.*

trait ValidatorZIO[T] {
  def validate(in: T): ZIO[Any, Nothing, Seq[String]]
}

object ValidatorZIO {
  def compose[T](
      validators: ValidatorZIO[T]*
  ): ValidatorZIO[T] =
    in => ZIO.collectAllPar(validators.map(_.validate(in))).map(_.flatten)

  def empty[T]: ValidatorZIO[T] =
    _ => ZIO.succeed(Seq.empty)

  def fromPure[T](v: Validator[T]): ValidatorZIO[T] =
    in => ZIO.succeed(v.validate(in))
}
