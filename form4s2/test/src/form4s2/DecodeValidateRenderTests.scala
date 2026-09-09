package form4s2

import utest.*
import zio.blocks.schema.Schema
import zio.http.{Form, FormField}
import zio.http.template2.*

case class Address(city: String, street: String)
object Address extends FormFields[Address]:
  given Schema[Address] = Schema.derived
  val city = field(_.city)
  val street = field(_.street)

case class Document(typ: String, number: String)
object Document extends FormFields[Document]:
  given Schema[Document] = Schema.derived
  val typ = field(_.typ)
  val number = field(_.number)

case class Reg(login: String, age: Int, address: Address, docs: Seq[Document])
object Reg extends FormFields[Reg]:
  given Schema[Reg] = Schema.derived
  val login = field(_.login)
  val age = field(_.age)
  val address = field(_.address)
  val docs = field(_.docs)

object Fixtures:

  def textInput(ctx: RenderContext[?]): Dom =
    input(name := ctx.path, `type` := "text")

  def numberInput(ctx: RenderContext[?]): Dom =
    input(name := ctx.path, `type` := "number")

  val addressForm: FormRenderer[Address] =
    draw(
      drawField(Address.city)(ctx => textInput(ctx)),
      drawField(Address.street)(ctx => textInput(ctx))
    )

  val documentForm: FormRenderer[Document] =
    draw(
      drawField(Document.typ)(ctx => textInput(ctx)),
      drawField(Document.number)(ctx => textInput(ctx))
    )

  val regForm: FormRenderer[Reg] =
    draw(
      drawField(Reg.login)(ctx => textInput(ctx)),
      drawField(Reg.age)(ctx => numberInput(ctx)),
      subForm(Reg.address, "Address")(addressForm),
      repeated(Reg.docs, "Docs")(documentForm)
    )

  given FormDecoder[Reg] = FormDecoder.derived

  given Validation[Reg] = Validation(
    rule(Reg.login, Validator.nonEmpty),
    rule(Reg.address / Address.city, Validator.nonEmpty),
    rule(Reg.address / Address.street, Validator.nonEmpty)
  )

object DecodeValidateRenderTests extends TestSuite:

  import Fixtures.{*, given}

  val tests = Tests {

    test("renders every field with lens-derived names") {
      val html = renderForm(regForm, None).render
      assert(html.contains("name=\"login\""))
      assert(html.contains("name=\"age\""))
      assert(html.contains("name=\"address.city\""))
      assert(html.contains("name=\"address.street\""))
      assert(html.contains("name=\"docs.0.typ\""))
      assert(html.contains("name=\"docs.0.number\""))
    }

    test("valid input decodes and validates") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Reg](form)
      assert(result.isRight)
      val r = result.toOption.get
      assert(r.login == "alice")
      assert(r.age == 30)
      assert(r.address.city == "NYC")
      assert(r.address.street == "Main St")
      assert(r.docs.isEmpty)
    }

    test("validation failure keeps old form and keys error by field name") {
      val form = Form(
        FormField.Simple("login", ""),
        FormField.Simple("age", "30"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Reg](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("login") == Seq("Поле должно быть заполнено"))
      assert(!incomplete.errors.contains("age"))
      assert(incomplete.oldForm.isDefined)
      assert(incomplete.oldForm.get.login == "")
    }

    test("decode failure has no old form") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "abc"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Reg](form)
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("age") == Seq("Невозможно преобразовать в число")
      )
      assert(incomplete.oldForm.isEmpty)
    }

    test("nested validation failure produces dot-notation key") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30"),
        FormField.Simple("address.city", ""),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Reg](form)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("address.city") == Seq("Поле должно быть заполнено")
      )
    }

    test("repeated subform decodes indexed fields") {
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St"),
        FormField.Simple("docs.0.typ", "passport"),
        FormField.Simple("docs.0.number", "12345"),
        FormField.Simple("docs.1.typ", "visa"),
        FormField.Simple("docs.1.number", "67890")
      )
      val result = decodeAndValidate[Reg](form)
      assert(result.isRight)
      val r = result.toOption.get
      assert(r.docs.length == 2)
      assert(r.docs(0).typ == "passport")
      assert(r.docs(1).number == "67890")
    }

    test("whole-model validator produces section-level error") {
      given Validation[Reg] = Validation(
        rule(Reg.login, Validator.nonEmpty),
        rule(Reg.address / Address.city, Validator.nonEmpty),
        rule(Reg.address / Address.street, Validator.nonEmpty),
        whole(
          Validator.custom[Reg]("City and street cannot be the same")(r =>
            r.address.city != r.address.street
          ),
          path = "address"
        )
      )
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "NYC")
      )
      val result = decodeAndValidate[Reg](form)
      val incomplete = result.swap.toOption.get
      assert(
        incomplete.errors("address") == Seq(
          "City and street cannot be the same"
        )
      )
    }

    test("repeated required rule fails on empty list") {
      given Validation[Reg] = Validation(
        rule(Reg.login, Validator.nonEmpty),
        rule(Reg.address / Address.city, Validator.nonEmpty),
        rule(Reg.address / Address.street, Validator.nonEmpty),
        rule(Reg.docs, Validator.nonEmptySeq)
      )
      val form = Form(
        FormField.Simple("login", "alice"),
        FormField.Simple("age", "30"),
        FormField.Simple("address.city", "NYC"),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[Reg](form)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("docs") == Seq("Добавьте минимум одно значение"))
    }

    test("conditional rendering via ambient env") {
      val viewOnly: FormRenderer[Reg] =
        draw(
          drawField(Reg.login)(ctx => textInput(ctx)),
          drawField(Reg.age)(ctx => numberInput(ctx))
            .showIf(_.env.mode == RenderEnv.Mode.Edit),
          subForm(Reg.address, "Address")(addressForm),
          repeated(Reg.docs, "Docs")(documentForm)
        )

      val editHtml = renderForm(viewOnly, None).render
      assert(editHtml.contains("name=\"age\""))

      val viewHtml = renderForm(viewOnly, None, env = RenderEnv.view).render
      assert(!viewHtml.contains("name=\"age\""))
      assert(viewHtml.contains("name=\"login\""))
    }
  }
