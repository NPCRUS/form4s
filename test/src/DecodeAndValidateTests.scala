package form4s

import utest.*
import zio.*
import zio.http.{Form, FormField}

case class AccountForm[F[_]](
    login: F[String],
    age: F[Int]
)

object DecodeAndValidateTests extends TestSuite {
  type AccountFormData = AccountForm[[T] =>> T]

  given AccountForm[HtmlForm.FieldSchema] = AccountForm(
    login = HtmlForm.FieldSchema(
      label = "Login",
      renderer = HtmlForm.stringRenderable,
      placeholderAttr = "Enter login",
      typeAttr = "text",
      validator = Validator.nonEmpty.toZIO
    ),
    age = HtmlForm.FieldSchema(
      label = "Age",
      renderer = HtmlForm.intRenderable,
      placeholderAttr = "Enter age",
      typeAttr = "number"
    )
  )

  def run[E, A](z: ZIO[Any, E, A]): Either[E, A] =
    Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.run(z.either)(using Trace.empty, u).getOrThrow()
    }

  val tests = Tests {
    test("valid input succeeds with decoded value") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30")
      )
      val result = run(HtmlForm.decodeAndValidate[AccountForm](form))
      assert(result.isRight)
      val user = result.toOption.get
      assert(user.login == "alice")
      assert(user.age == 30)
    }

    test("validation failure fails with errors and keeps old form") {
      val form = Form(
        FormField.Simple("login", ""),
        FormField.Simple("age", "30")
      )
      val result = run(HtmlForm.decodeAndValidate[AccountForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("login") == Seq("Поле должно быть заполнено"))
      assert(!incomplete.errors.contains("age"))
      assert(incomplete.oldForm.isDefined)
      assert(incomplete.oldForm.get.login == "")
      assert(incomplete.oldForm.get.age == 30)
    }

    test("decode failure fails with errors and no old form") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "abc")
      )
      val result = run(HtmlForm.decodeAndValidate[AccountForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("age") == Seq("Невозможно преобразовать в число")
      )
      assert(incomplete.oldForm.isEmpty)
    }
  }
}
