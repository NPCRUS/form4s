package form4s

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

  // ---- user-side rendering helpers (library knows nothing about these) ----

  def textInput(ctx: RenderContext[?]): List[Dom] =
    List(input(name := ctx.path, `type` := "text"))

  def numberInput(ctx: RenderContext[?]): List[Dom] =
    List(input(name := ctx.path, `type` := "number"))

  def section(label: String)(children: List[Dom]): List[Dom] =
    List(fieldset(legend(text(label)), children))

  def repeatedSection(
      label: String,
      items: List[Dom],
      tpl: List[Dom]
  ): List[Dom] =
    List(
      fieldset(
        legend(text(label)),
        items,
        template(tpl),
        button(`type` := "button", text("Add"))
      )
    )

  // ---- forms ----

  val addressForm: FormRenderer[Address, List[Dom]] =
    draw(
      drawField(Address.city)(ctx => textInput(ctx)),
      drawField(Address.street)(ctx => textInput(ctx))
    )(_ ++ _)

  val documentForm: FormRenderer[Document, List[Dom]] =
    draw(
      drawField(Document.typ)(ctx => textInput(ctx)),
      drawField(Document.number)(ctx => textInput(ctx))
    )(_ ++ _)

  def docsSection(ctx: RenderContext[Seq[Document]]): List[Dom] =
    val items = ctx.oldValue.getOrElse(Seq.empty)
    val rendered = items.zipWithIndex.flatMap { (doc, i) =>
      documentForm(RenderContext(ctx.at(i), Some(doc), ctx.errors))
    }.toList
    val tpl = documentForm(RenderContext(ctx.at(0), None, Map.empty))
    repeatedSection("Docs", rendered, tpl)

  def regForm(isEdit: Boolean): FormRenderer[Reg, List[Dom]] =
    draw(
      drawField(Reg.login)(ctx => textInput(ctx)),
      drawField(Reg.age)(ctx => if isEdit then numberInput(ctx) else Nil),
      drawField(Reg.address)(ctx => section("Address")(addressForm(ctx))),
      drawField(Reg.docs)(ctx => docsSection(ctx))
    )(_ ++ _)

  // ---- decode / validate ----

  given FormDecoder[Reg] = FormDecoder.derived

  given Validation[Reg] = Validation(
    rule(Reg.login, Validator.nonEmpty),
    rule(Reg.address / Address.city, Validator.nonEmpty),
    rule(Reg.address / Address.street, Validator.nonEmpty)
  )

object DecodeValidateRenderTests extends TestSuite:

  import Fixtures.{*, given}

  def html(fields: List[Dom]): String = fields.map(_.render).mkString

  val tests = Tests {

    test("renders every field with lens-derived names") {
      val out = html(renderForm(regForm(true), None))
      assert(out.contains("name=\"login\""))
      assert(out.contains("name=\"age\""))
      assert(out.contains("name=\"address.city\""))
      assert(out.contains("name=\"address.street\""))
      assert(out.contains("name=\"docs.0.typ\""))
      assert(out.contains("name=\"docs.0.number\""))
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

    test("conditional rendering via captured flag") {
      val editHtml = html(renderForm(regForm(true), None))
      assert(editHtml.contains("name=\"age\""))

      val viewHtml = html(renderForm(regForm(false), None))
      assert(!viewHtml.contains("name=\"age\""))
      assert(viewHtml.contains("name=\"login\""))
    }
  }
