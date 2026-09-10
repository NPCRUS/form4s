package form4s

import zio.*

/** Effectful validator. Runs validation effects concurrently via
  * `ZIO.collectAllPar`.
  *
  * Pure [[Validator]] instances can be lifted via [[fromPure]] or the
  * [[Validator.toZIO]] convenience method.
  *
  * @tparam T
  *   the type of value being validated
  */
trait ValidatorZIO[T]:
  def validate(in: T): ZIO[Any, Nothing, Seq[String]]

  def ++(that: ValidatorZIO[T]): ValidatorZIO[T] =
    ValidatorZIO.compose(this, that)

  def contramap[U](f: U => T): ValidatorZIO[U] = in => validate(f(in))

  def map(f: T => T): ValidatorZIO[T] = in => validate(f(in))

  def option: ValidatorZIO[Option[T]] =
    in => in.fold(ZIO.succeed(Seq.empty))(validate)

object ValidatorZIO:

  def compose[T](validators: ValidatorZIO[T]*): ValidatorZIO[T] =
    in => ZIO.collectAllPar(validators.map(_.validate(in))).map(_.flatten)

  def empty[T]: ValidatorZIO[T] = _ => ZIO.succeed(Seq.empty)

  def fromPure[T](v: Validator[T]): ValidatorZIO[T] =
    in => ZIO.succeed(v.validate(in))
