package form4s

import zio.*

/** Effectful validator. Runs validation effects concurrently via
  * `ZIO.collectAllPar`.
  *
  * The environment type `R` is contravariant (mirroring `ZIO[-R, +E, +A]`), so
  * a validator requiring `Any` (e.g. one lifted from a pure [[Validator]] via
  * [[fromPure]]) is a subtype of one requiring any environment and auto-widens
  * wherever an env-ful validator is expected. This lets you freely mix pure and
  * effectful validators in a single [[rule]]/[[Validation]].
  *
  * @tparam T
  *   the type of value being validated
  * @tparam R
  *   the ZIO environment the validation effect requires (e.g. a service or
  *   resource used during validation)
  */
trait ValidatorZIO[T, -R]:
  def validate(in: T): ZIO[R, Nothing, Seq[String]]

  /** Concatenate with another validator for the same input type, combining
    * their environments (the result requires the more specific `R1`).
    */
  def ++[R1 <: R](that: ValidatorZIO[T, R1]): ValidatorZIO[T, R1] =
    ValidatorZIO.compose(this, that)

  /** Adapt this validator to a different input type by applying `f` first. */
  def contramap[U](f: U => T): ValidatorZIO[U, R] = in => validate(f(in))

  /** Transform the input before validation. */
  def map(f: T => T): ValidatorZIO[T, R] = in => validate(f(in))

  /** Lift to [[Option]]: only checks the value when it is `Some`. */
  def option: ValidatorZIO[Option[T], R] =
    in => in.fold(ZIO.succeed(Seq.empty))(validate)

object ValidatorZIO:

  /** Combine validators concurrently, concatenating their error messages. */
  def compose[T, R](validators: ValidatorZIO[T, R]*): ValidatorZIO[T, R] =
    in => ZIO.collectAllPar(validators.map(_.validate(in))).map(_.flatten)

  /** A validator that always passes. */
  def empty[T]: ValidatorZIO[T, Any] = _ => ZIO.succeed(Seq.empty)

  /** Lift a pure [[Validator]] into an effectful validator. */
  def fromPure[T](v: Validator[T]): ValidatorZIO[T, Any] =
    in => ZIO.succeed(v.validate(in))
