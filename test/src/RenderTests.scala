package form4s

import utest.*
import zio.blocks.schema.Schema
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

object RenderTests extends TestSuite:

  import Fixtures.*

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

    test("conditional rendering via captured flag") {
      val editHtml = html(renderForm(regForm(true), None))
      assert(editHtml.contains("name=\"age\""))

      val viewHtml = html(renderForm(regForm(false), None))
      assert(!viewHtml.contains("name=\"age\""))
      assert(viewHtml.contains("name=\"login\""))
    }
  }
