package form4s

import utest.*
import zio.blocks.schema.Schema
import zio.http.{Form, FormField}
import zio.http.template2.*

case class AutoAddress(city: String, street: String)

object AutoAddress extends AutoFields[AutoAddress]:
  given Schema[AutoAddress] = Schema.derived

case class AutoReg(
    username: String,
    email: String,
    age: Option[Int],
    address: AutoAddress
)

object AutoReg extends AutoFields[AutoReg]:
  given Schema[AutoReg] = Schema.derived

object AutoFieldsTests extends TestSuite:

  val autoAddressForm: FormRenderer[AutoAddress, List[Dom]] =
    draw(
      drawField(AutoAddress.city)(ctx => List[Dom](input(name := ctx.path))),
      drawField(AutoAddress.street)(ctx => List[Dom](input(name := ctx.path)))
    )(_ ++ _)

  val tests = Tests {

    test("auto-derives literal-typed leaf fields") {
      val u: LeafField[AutoReg, String, "username"] = AutoReg.username
      val e: LeafField[AutoReg, String, "email"] = AutoReg.email
      val a: LeafField[AutoReg, Option[Int], "age"] = AutoReg.age
      val ad: LeafField[AutoReg, AutoAddress, "address"] = AutoReg.address
      val c: LeafField[AutoAddress, String, "city"] = AutoAddress.city

      assert(u.name == "username")
      assert(e.name == "email")
      assert(a.name == "age")
      assert(ad.name == "address")
      assert(c.name == "city")

      val reg = AutoReg("alice", "a@b.c", None, AutoAddress("NYC", "Main St"))
      assert(AutoReg.username.get(reg) == "alice")
      assert(AutoReg.address.get(reg) == AutoAddress("NYC", "Main St"))
      assert(AutoAddress.city.get(reg.address) == "NYC")
    }

    test("renders with lens-derived names and totality") {
      val form = draw(
        drawField(AutoReg.username)(ctx => List[Dom](input(name := ctx.path))),
        drawField(AutoReg.email)(ctx => List[Dom](input(name := ctx.path))),
        drawField(AutoReg.age)(ctx => List[Dom](input(name := ctx.path))),
        drawField(AutoReg.address)(ctx => autoAddressForm(ctx))
      )(_ ++ _)

      val html = renderForm(form, None).map(_.render).mkString
      assert(html.contains("name=\"username\""))
      assert(html.contains("name=\"email\""))
      assert(html.contains("name=\"age\""))
      assert(html.contains("name=\"address.city\""))
      assert(html.contains("name=\"address.street\""))
    }

    test("validation uses auto-derived fields") {
      given FormDecoder[AutoReg] = FormDecoder.derived
      given Validation[AutoReg] = Validation(
        rule(AutoReg.username, Validator.nonEmpty),
        rule(AutoReg.email, Validator.isEmail),
        rule(AutoReg.address / AutoAddress.city, Validator.nonEmpty)
      )

      val form = Form(
        FormField.Simple("username", "alice"),
        FormField.Simple("email", "not-an-email"),
        FormField.Simple("address.city", ""),
        FormField.Simple("address.street", "Main St")
      )
      val result = decodeAndValidate[AutoReg](form)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("email") == Seq("Некорректный email"))
      assert(
        incomplete.errors("address.city") == Seq("Поле должно быть заполнено")
      )
    }
  }
