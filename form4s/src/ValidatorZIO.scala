package form4s

import zio.*

/** Effectful validator. Runs validation effects concurrently via
  * `ZIO.collectAllPar`.
  *
  * Pure [[Validator]] instances can be lifted via [[fromPure]] or the
  * [[Validator.toZIO]] convenience method. [[rule]]/[[whole]] accept both pure
  * and effectful validators (pure ones auto-lift).
  *
  * @tparam T
  *   the type of value being validated
  */
trait ValidatorZIO[T]:
  def validate(in: T): ZIO[Any, Nothing, Seq[String]]

  /** Concatenate with another validator for the same input type. */
  def ++(that: ValidatorZIO[T]): ValidatorZIO[T] =
    ValidatorZIO.compose(this, that)

  /** Adapt this validator to a different input type by applying `f` first. */
  def contramap[U](f: U => T): ValidatorZIO[U] = in => validate(f(in))

  /** Transform the input before validation. */
  def map(f: T => T): ValidatorZIO[T] = in => validate(f(in))

  /** Lift to [[Option]]: only checks the value when it is `Some`. */
  def option: ValidatorZIO[Option[T]] =
    in => in.fold(ZIO.succeed(Seq.empty))(validate)

object ValidatorZIO:

  /** Combine validators concurrently, concatenating their error messages. */
  def compose[T](validators: ValidatorZIO[T]*): ValidatorZIO[T] =
    in => ZIO.collectAllPar(validators.map(_.validate(in))).map(_.flatten)

  /** A validator that always passes. */
  def empty[T]: ValidatorZIO[T] = _ => ZIO.succeed(Seq.empty)

  /** Lift a pure [[Validator]] into an effectful validator. */
  def fromPure[T](v: Validator[T]): ValidatorZIO[T] =
    in => ZIO.succeed(v.validate(in))
