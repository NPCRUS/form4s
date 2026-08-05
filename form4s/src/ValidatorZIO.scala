package form4s

import zio.*

/** Effectful validator used by [[FieldSchema]] during form validation. Runs
  * validation effects concurrently via `ZIO.collectAllPar`.
  *
  * Pure [[Validator]] instances can be lifted via [[fromPure]] or the
  * [[Validator.toZIO]] convenience method.
  *
  * @note
  *   All error messages are in Russian (see [[Validator]] for details).
  * @tparam T
  *   the type of value being validated
  */
trait ValidatorZIO[T] { that =>

  /** Run the validation, returning a list of error messages (empty = valid).
    * The effect is infallible — errors are returned as values, never failed.
    */
  def validate(in: T): ZIO[Any, Nothing, Seq[String]]

  /** Transform the input type before validation. */
  def contramap[U](f: U => T): ValidatorZIO[U] =
    in => that.validate(f(in))

  /** Transform the input value before validation (identity by default; useful
    * for normalisation).
    */
  def map(f: T => T): ValidatorZIO[T] =
    in => that.validate(f(in))
}

object ValidatorZIO {

  /** Compose multiple validators: all are run concurrently and their error
    * messages are concatenated.
    */
  def compose[T](
      validators: ValidatorZIO[T]*
  ): ValidatorZIO[T] =
    in => ZIO.collectAllPar(validators.map(_.validate(in))).map(_.flatten)

  /** A validator that always passes (returns no errors). */
  def empty[T]: ValidatorZIO[T] =
    _ => ZIO.succeed(Seq.empty)

  /** Lift a pure [[Validator]] into an effectful validator. */
  def fromPure[T](v: Validator[T]): ValidatorZIO[T] =
    in => ZIO.succeed(v.validate(in))
}
