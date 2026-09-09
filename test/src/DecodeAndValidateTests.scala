package form4s

import utest.*
import zio.blocks.schema.Schema
import zio.http.{Form, FormField}

case class Account(login: String, age: Int)
object Account extends AutoFields[Account]:
  given Schema[Account] = Schema.derived

case class Addr(city: String, street: String)
object Addr extends AutoFields[Addr]:
  given Schema[Addr] = Schema.derived

case class Doc(typ: String, number: String)
object Doc extends AutoFields[Doc]:
  given Schema[Doc] = Schema.derived

case class Signup(login: String, address: Addr, docs: Seq[Doc])
object Signup extends AutoFields[Signup]:
  given Schema[Signup] = Schema.derived

case class OptForm(name: String, opt: Option[String])
object OptForm extends AutoFields[OptForm]:
  given Schema[OptForm] = Schema.derived

case class OptOnly(opt: Option[String])
object OptOnly extends AutoFields[OptOnly]:
  given Schema[OptOnly] = Schema.derived

object DecodeAndValidateTests extends TestSuite:

  given FormDecoder[Account] = FormDecoder.derived
  given FormDecoder[Addr] = FormDecoder.derived
  given FormDecoder[Doc] = FormDecoder.derived
  given FormDecoder[Signup] = FormDecoder.derived
  given FormDecoder[OptForm] = FormDecoder.derived
  given FormDecoder[OptOnly] = FormDecoder.derived

  given Validation[Account] = Validation(
    rule(Account.login, Validator.nonEmpty)
  )

  given Validation[Signup] = Validation(
    rule(Signup.login, Validator.nonEmpty),
    rule(Signup.address / Addr.city, Validator.nonEmpty),
    rule(Signup.address / Addr.street, Validator.nonEmpty),
    ruleEach(
      Signup.docs,
      Validation(
        rule(Doc.typ, Validator.nonEmpty),
        rule(Doc.number, Validator.nonEmpty)
      )
    )
  )

  val tests = Tests {

    test("valid input succeeds with decoded value") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30")
      )
      val result = decodeAndValidate[Account](form)
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
      val result = decodeAndValidate[Account](form)
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
      val result = decodeAndValidate[Account](form)
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
      val result = decodeAndValidate[Signup](form)
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
      val result = decodeAndValidate[Signup](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("address.city") == Seq("Поле должно быть заполнено")
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
      val result = decodeAndValidate[Signup](form)
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
      val result = decodeAndValidate[Signup](form)
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
      val result = decodeAndValidate[Signup](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("docs.0.number") == Seq("Поле должно быть заполнено")
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
      val result = decodeAndValidate[Signup](form)
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
      val result = decodeAndValidate[Signup](form)
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
      val result = decodeAndValidate[Signup](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("docs.0.typ") == Seq("Обязательное поле"))
      assert(incomplete.oldForm.isEmpty)
    }

    test("custom SubForm validator error appears at subform path") {
      given Validation[Signup] = Validation(
        rule(Signup.login, Validator.nonEmpty),
        rule(Signup.address / Addr.city, Validator.nonEmpty),
        rule(Signup.address / Addr.street, Validator.nonEmpty),
        whole(
          Validator.custom[Signup]("City and street cannot be the same")(s =>
            s.address.city != s.address.street
          ),
          path = "address"
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "NYC")
      )
      val result = decodeAndValidate[Signup](form)
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

    test("repeated required rule, empty list fails validation") {
      given Validation[Signup] = Validation(
        rule(Signup.login, Validator.nonEmpty),
        rule(Signup.address / Addr.city, Validator.nonEmpty),
        rule(Signup.address / Addr.street, Validator.nonEmpty),
        rule(Signup.docs, Validator.nonEmptySeq)
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Signup](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("docs") == Seq("Добавьте минимум одно значение")
      )
    }

    test("repeated required rule, non-empty list passes validation") {
      given Validation[Signup] = Validation(
        rule(Signup.login, Validator.nonEmpty),
        rule(Signup.address / Addr.city, Validator.nonEmpty),
        rule(Signup.address / Addr.street, Validator.nonEmpty),
        rule(Signup.docs, Validator.nonEmptySeq)
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", "12345")
      )
      val result = decodeAndValidate[Signup](form)
      assert(result.isRight)
    }

    test("custom repeated validator error appears at section path") {
      given Validation[Signup] = Validation(
        rule(Signup.login, Validator.nonEmpty),
        rule(Signup.address / Addr.city, Validator.nonEmpty),
        rule(Signup.address / Addr.street, Validator.nonEmpty),
        rule(
          Signup.docs,
          Validator.custom[Seq[Doc]]("Maximum 2 documents")(_.length <= 2)
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
      val result = decodeAndValidate[Signup](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("docs") == Seq("Maximum 2 documents"))
      assert(incomplete.oldForm.isDefined)
    }

    test("custom repeated validator composes with required") {
      given Validation[Signup] = Validation(
        rule(Signup.login, Validator.nonEmpty),
        rule(Signup.address / Addr.city, Validator.nonEmpty),
        rule(Signup.address / Addr.street, Validator.nonEmpty),
        rule(
          Signup.docs,
          Validator.nonEmptySeq[Doc] ++
            Validator.custom[Seq[Doc]]("Maximum 2 documents")(_.length <= 2)
        )
      )

      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Signup](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      val errs = incomplete.errors("docs")
      assert(errs.contains("Добавьте минимум одно значение"))
    }

    test("String field with no rule passes empty value") {
      given Validation[Account] = Validation()

      val form = Form(
        FormField.Simple("login", ""),
        FormField.Simple("age", "30")
      )
      val result = decodeAndValidate[Account](form)
      assert(result.isRight)
    }

    test(
      "Option[String] field absent from body decodes to None without error"
    ) {
      given Validation[OptForm] = Validation(
        rule(OptForm.name, Validator.nonEmpty)
      )

      val form = Form(FormField.Simple("name", "alice"))
      val result = decodeAndValidate[OptForm](form)
      assert(result.isRight)
      assert(result.toOption.get.opt == None)
    }

    test("Option[String] field with required rule fails on None") {
      given Validation[OptOnly] = Validation(
        rule(OptOnly.opt, Validator.required)
      )

      val result = decodeAndValidate[OptOnly](Form.empty)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("opt") == Seq("Обязательное поле"))
    }
  }
