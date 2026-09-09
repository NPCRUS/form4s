package form4s

import utest.*
import zio.blocks.schema.Schema
import zio.http.{Form, FormField}
import zio.http.template2.*

enum Color:
  case Red, Green, Blue

object Color:
  given FormDecoder[Color] = new FormDecoder[Color]:
    def decode(form: Form): Either[Seq[DecodingError], Color] =
      form.formData.headOption.flatMap(_.stringValue) match
        case Some(s) =>
          Color.values
            .find(_.toString == s)
            .toRight(Seq(DecodingError("", "Нет такого варианта выбора")))
        case None => Left(Seq(DecodingError("", "Обязательное поле")))

case class WithEnum(color: Color, label: String)

object WithEnum extends AutoFields[WithEnum]:
  given Schema[WithEnum] = Schema.derived

case class ManualEnum(c: Color, n: Int)

object ManualEnum extends FormFields[ManualEnum]:
  given Schema[ManualEnum] = Schema.derived
  val c = field(_.c)
  val n = field(_.n)

case class Ov(a: String, b: String)

object Ov extends AutoFields[Ov]:
  given Schema[Ov] = Schema.derived
  val a = field(_.a)

case class Box[A](value: A)

object MacroRegressionTests extends TestSuite:

  val tests = Tests {

    test("AutoFields derives an enum-typed field") {
      val c: LeafField[WithEnum, Color, "color"] = WithEnum.color
      val l: LeafField[WithEnum, String, "label"] = WithEnum.label
      assert(c.name == "color")
      assert(l.name == "label")
      assert(WithEnum.color.get(WithEnum(Color.Red, "x")) == Color.Red)
    }

    test("AutoFields + enum member renders with lens-derived names") {
      val form = draw(
        drawField(WithEnum.color)(ctx => List[Dom](input(name := ctx.path))),
        drawField(WithEnum.label)(ctx => List[Dom](input(name := ctx.path)))
      )(_ ++ _)
      val html = renderForm(form, None).map(_.render).mkString
      assert(html.contains("name=\"color\""))
      assert(html.contains("name=\"label\""))
    }

    test("AutoFields + enum member decodes via manual FormDecoder") {
      given FormDecoder[WithEnum] = FormDecoder.derived
      val form = Form(
        FormField.Simple("color", "Green"),
        FormField.Simple("label", "hello")
      )
      val result = FormDecoder.decode[WithEnum](form)
      assert(result.isRight)
      assert(result.toOption.get.color == Color.Green)
      assert(result.toOption.get.label == "hello")
    }

    test("field(_.x) works on an enum-typed field") {
      val c: LeafField[ManualEnum, Color, "c"] = ManualEnum.c
      val n: LeafField[ManualEnum, Int, "n"] = ManualEnum.n
      assert(ManualEnum.c.get(ManualEnum(Color.Blue, 1)) == Color.Blue)
      assert(ManualEnum.n.get(ManualEnum(Color.Blue, 1)) == 1)
    }

    test("manual field overrides AutoFields derivation") {
      val a: LeafField[Ov, String, "a"] = Ov.a
      val b: LeafField[Ov, String, "b"] = Ov.b
      assert(a.name == "a")
      assert(b.name == "b")
      assert(Ov.a.get(Ov("x", "y")) == "x")
      assert(Ov.b.get(Ov("x", "y")) == "y")
    }

    test("generic case class via FieldsFor") {
      given Schema[Box[Int]] = Schema.derived
      val fields = summon[FieldsFor[Box[Int]]].fields
      val v: LeafField[Box[Int], Int, "value"] = fields.value
      assert(v.name == "value")
      assert(v.get(Box(42)) == 42)
    }
  }
