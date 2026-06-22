package form4s

import utest.*
import zio.*

object ValidatorZIOTests extends TestSuite {
  def run[A](z: ZIO[Any, Nothing, A]): A =
    Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.run(z)(using Trace.empty, u).getOrThrow()
    }

  val tests = Tests {
    test("empty validator gives no notes") {
      assert(run(ValidatorZIO.empty[String].validate("I'm a teapot ☕")).isEmpty)
    }

    test("fromPure lifts pure judgment into the ZIO realm") {
      assert(run(Validator.nonEmpty.toZIO.validate("covfefe")).isEmpty)
      assert(
        run(Validator.nonEmpty.toZIO.validate("")) == Seq(
          "Пустое поле запрещено"
        )
      )
    }

    test("compose roasts a weak password in parallel") {
      val judge = ValidatorZIO.compose(
        Validator.minLength(8).toZIO,
        Validator.matches("""\d+""".r).toZIO
      )
      val complaints = run(judge.validate("hunter2"))
      assert(complaints.contains("Минимум 8 символов"))
      assert(complaints.contains("Неверный формат"))
    }

    test("a custom effectful validator that keeps saying нет") {
      val areWeThereYet: ValidatorZIO[String] = in =>
        ZIO.succeed(if (in == "are we there yet") Seq("Нет") else Seq.empty)
      assert(run(areWeThereYet.validate("are we there yet")) == Seq("Нет"))
      assert(run(areWeThereYet.validate("5 more minutes")).isEmpty)
    }
  }
}
