package form4s

import utest.*
import zio.*
import zio.http.{Form, FormField}

case class AccountForm[F[_]](
    login: F[String],
    age: F[Int]
)

case class AddressF[F[_]](
    city: F[String],
    street: F[String]
)

case class DocF[F[_]](
    typ: F[String],
    number: F[String]
)

case class RegForm[F[_]](
    login: F[String],
    address: F[AddressF[F]],
    docs: F[Seq[DocF[F]]]
)

object DecodeAndValidateTests extends TestSuite {
  type AccountFormData = AccountForm[[T] =>> T]

  given AccountForm[HtmlForm.FormSchema] = AccountForm(
    login = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "Login",
        renderer = HtmlForm.stringRenderable,
        placeholderAttr = "Enter login",
        typeAttr = "text"
      )
    ),
    age = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "Age",
        renderer = HtmlForm.intRenderable,
        placeholderAttr = "Enter age",
        typeAttr = "number"
      )
    )
  )

  given AddressF[HtmlForm.FormSchema] = AddressF(
    city = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "City",
        renderer = HtmlForm.stringRenderable,
        placeholderAttr = "city",
        typeAttr = "text"
      )
    ),
    street = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "Street",
        renderer = HtmlForm.stringRenderable,
        placeholderAttr = "street",
        typeAttr = "text"
      )
    )
  )

  given DocF[HtmlForm.FormSchema] = DocF(
    typ = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "Type",
        renderer = HtmlForm.stringRenderable,
        placeholderAttr = "type",
        typeAttr = "text"
      )
    ),
    number = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "Number",
        renderer = HtmlForm.stringRenderable,
        placeholderAttr = "number",
        typeAttr = "text"
      )
    )
  )

  given RegForm[HtmlForm.FormSchema] = RegForm(
    login = HtmlForm.FormSchema.Field(
      HtmlForm.FieldSchema(
        label = "Login",
        renderer = HtmlForm.stringRenderable,
        placeholderAttr = "Enter login",
        typeAttr = "text"
      )
    ),
    address = HtmlForm.FormSchema.SubForm(
      "Address",
      summon[AddressF[HtmlForm.FormSchema]]
    ),
    docs = HtmlForm.FormSchema.RepeatedSubForm(
      "Docs",
      summon[DocF[HtmlForm.FormSchema]]
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

    test("valid subform data decodes and validates") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isRight)
      val reg = result.toOption.get
      assert(reg.login == "alice")
      assert(reg.address.city == "NYC")
      assert(reg.address.street == "Main St")
      assert(reg.docs.isEmpty)
    }

    test("subform validation failure produces dot-notation error key") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", ""),
        FormField.Simple("address.street", "Main St")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("address.city") == Seq(
          "Поле должно быть заполнено"
        )
      )
      assert(!incomplete.errors.contains("address.street"))
      assert(!incomplete.errors.contains("login"))
    }

    test("subform old form preserved on validation failure") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", ""),
        FormField.Simple("address.street", "Main St")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      val incomplete = result.swap.toOption.get
      assert(incomplete.oldForm.isDefined)
      val old = incomplete.oldForm.get
      assert(old.login == "alice")
      assert(old.address.city == "")
      assert(old.address.street == "Main St")
    }

    test("valid repeated subform data decodes and validates") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", "12345"),
        FormField.Simple("docs.1.typ", "visa"),
        FormField.Simple("docs.1.number", "67890")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isRight)
      val reg = result.toOption.get
      assert(reg.login == "alice")
      assert(reg.docs.length == 2)
      assert(reg.docs(0).typ == "passport")
      assert(reg.docs(0).number == "12345")
      assert(reg.docs(1).typ == "visa")
      assert(reg.docs(1).number == "67890")
    }

    test("repeated subform validation failure produces indexed error key") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", ""),
        FormField.Simple("docs.1.typ", "visa"),
        FormField.Simple("docs.1.number", "67890")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("docs.0.number") == Seq(
          "Поле должно быть заполнено"
        )
      )
      assert(!incomplete.errors.contains("docs.0.typ"))
      assert(!incomplete.errors.contains("docs.1.number"))
      assert(!incomplete.errors.contains("login"))
    }

    test("repeated subform old form preserved on validation failure") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", ""),
        FormField.Simple("docs.1.typ", "visa"),
        FormField.Simple("docs.1.number", "67890")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      val incomplete = result.swap.toOption.get
      assert(incomplete.oldForm.isDefined)
      val old = incomplete.oldForm.get
      assert(old.login == "alice")
      assert(old.docs.length == 2)
      assert(old.docs(0).typ == "passport")
      assert(old.docs(0).number == "")
      assert(old.docs(1).typ == "visa")
      assert(old.docs(1).number == "67890")
    }

    test("decode failure in subform produces correct error") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("address.street") == Seq("Обязательное поле")
      )
      assert(incomplete.oldForm.isEmpty)
    }

    test("decode failure in repeated subform produces correct error") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.number", "12345")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("docs.0.typ") == Seq("Обязательное поле")
      )
      assert(incomplete.oldForm.isEmpty)
    }

    test("custom SubForm validator error appears at subform path") {
      given RegForm[HtmlForm.FormSchema] = RegForm(
        login = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Login",
            renderer = HtmlForm.stringRenderable,
            placeholderAttr = "Enter login",
            typeAttr = "text"
          )
        ),
        address = HtmlForm.FormSchema.SubForm(
          "Address",
          summon[AddressF[HtmlForm.FormSchema]],
          validator = Validator
            .custom[AddressF[[T] =>> T]]("City and street cannot be the same")(
              a => a.city != a.street
            )
            .toZIO
        ),
        docs = HtmlForm.FormSchema.RepeatedSubForm(
          "Docs",
          summon[DocF[HtmlForm.FormSchema]]
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "NYC")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("address") == Seq(
          "City and street cannot be the same"
        )
      )
      assert(!incomplete.errors.contains("address.city"))
      assert(!incomplete.errors.contains("address.street"))
      assert(incomplete.oldForm.isDefined)
    }

    test("RepeatedSubForm required=true, empty list fails validation") {
      given RegForm[HtmlForm.FormSchema] = RegForm(
        login = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Login",
            renderer = HtmlForm.stringRenderable,
            placeholderAttr = "Enter login",
            typeAttr = "text"
          )
        ),
        address = HtmlForm.FormSchema.SubForm(
          "Address",
          summon[AddressF[HtmlForm.FormSchema]]
        ),
        docs = HtmlForm.FormSchema.RepeatedSubForm(
          "Docs",
          summon[DocF[HtmlForm.FormSchema]],
          required = true
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("docs") == Seq("Добавьте минимум одно значение")
      )
    }

    test("RepeatedSubForm required=true, non-empty list passes validation") {
      given RegForm[HtmlForm.FormSchema] = RegForm(
        login = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Login",
            renderer = HtmlForm.stringRenderable,
            placeholderAttr = "Enter login",
            typeAttr = "text"
          )
        ),
        address = HtmlForm.FormSchema.SubForm(
          "Address",
          summon[AddressF[HtmlForm.FormSchema]]
        ),
        docs = HtmlForm.FormSchema.RepeatedSubForm(
          "Docs",
          summon[DocF[HtmlForm.FormSchema]],
          required = true
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", "12345")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isRight)
    }

    test("custom RepeatedSubForm validator error appears at section path") {
      given RegForm[HtmlForm.FormSchema] = RegForm(
        login = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Login",
            renderer = HtmlForm.stringRenderable,
            placeholderAttr = "Enter login",
            typeAttr = "text"
          )
        ),
        address = HtmlForm.FormSchema.SubForm(
          "Address",
          summon[AddressF[HtmlForm.FormSchema]]
        ),
        docs = HtmlForm.FormSchema.RepeatedSubForm(
          "Docs",
          summon[DocF[HtmlForm.FormSchema]],
          validator = Validator
            .custom[Seq[DocF[[T] =>> T]]]("Maximum 2 documents")(
              _.length <= 2
            )
            .toZIO
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", "1"),
        FormField.Simple("docs.1.typ", "visa"),
        FormField.Simple("docs.1.number", "2"),
        FormField.Simple("docs.2.typ", "id"),
        FormField.Simple("docs.2.number", "3")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("docs") == Seq("Maximum 2 documents")
      )
      assert(incomplete.oldForm.isDefined)
    }

    test("custom RepeatedSubForm validator composes with required") {
      given RegForm[HtmlForm.FormSchema] = RegForm(
        login = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Login",
            renderer = HtmlForm.stringRenderable,
            placeholderAttr = "Enter login",
            typeAttr = "text"
          )
        ),
        address = HtmlForm.FormSchema.SubForm(
          "Address",
          summon[AddressF[HtmlForm.FormSchema]]
        ),
        docs = HtmlForm.FormSchema.RepeatedSubForm(
          "Docs",
          summon[DocF[HtmlForm.FormSchema]],
          required = true,
          validator = Validator
            .custom[Seq[DocF[[T] =>> T]]]("Maximum 2 documents")(
              _.length <= 2
            )
            .toZIO
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = run(HtmlForm.decodeAndValidate[RegForm](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      val errs = incomplete.errors("docs")
      assert(errs.contains("Добавьте минимум одно значение"))
    }

    test("String field with required=false passes empty value") {
      given AccountForm[HtmlForm.FormSchema] = AccountForm(
        login = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Login",
            renderer = HtmlForm.stringRenderable,
            placeholderAttr = "Enter login",
            typeAttr = "text",
            required = false
          )
        ),
        age = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Age",
            renderer = HtmlForm.intRenderable,
            placeholderAttr = "Enter age",
            typeAttr = "number"
          )
        )
      )

      val form = Form(
        FormField.Simple("login", ""),
        FormField.Simple("age", "30")
      )
      val result = run(HtmlForm.decodeAndValidate[AccountForm](form))
      assert(result.isRight)
    }

    test("Option[String] field with required=true fails on None") {
      case class OptForm[F[_]](opt: F[Option[String]])

      given OptForm[HtmlForm.FormSchema] = OptForm(
        opt = HtmlForm.FormSchema.Field(
          HtmlForm.FieldSchema(
            label = "Optional field",
            renderer = HtmlForm.stringRenderable.optional,
            placeholderAttr = "",
            typeAttr = "text",
            required = true
          )
        )
      )

      val result = run(HtmlForm.decodeAndValidate[OptForm](Form.empty))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("opt") == Seq("Обязательное поле")
      )
    }
  }
}
