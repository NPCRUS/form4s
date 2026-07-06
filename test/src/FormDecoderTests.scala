package form4s

import utest._
import zio.http.{Form, FormField, MediaType}
import zio.Chunk
import java.time.LocalDateTime
import java.util.UUID

object FormDecoderTests extends TestSuite {
  val tests = Tests {
    test("decode String from simple field") {
      val form = Form(FormField.Simple("test", "hello"))
      val decoded = summon[FormDecoder[String]].decode(form)
      assert(decoded == Right("hello"))
    }

    test("decode Int from simple field") {
      val form = Form(FormField.Simple("test", "42"))
      val decoded = summon[FormDecoder[Int]].decode(form)
      assert(decoded == Right(42))
    }

    test("decode Int fails on non-numeric string") {
      val form = Form(FormField.Simple("test", "not-a-number"))
      val decoded = summon[FormDecoder[Int]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode Boolean true from simple field") {
      val form = Form(FormField.Simple("test", "true"))
      val decoded = summon[FormDecoder[Boolean]].decode(form)
      assert(decoded == Right(true))
    }

    test("decode Boolean false from simple field") {
      val form = Form(FormField.Simple("test", "false"))
      val decoded = summon[FormDecoder[Boolean]].decode(form)
      assert(decoded == Right(false))
    }

    test("decode Boolean from on/off/1/0") {
      assert(
        summon[FormDecoder[Boolean]].decode(
          Form(FormField.Simple("", "on"))
        ) == Right(true)
      )
      assert(
        summon[FormDecoder[Boolean]].decode(
          Form(FormField.Simple("", "off"))
        ) == Right(false)
      )
      assert(
        summon[FormDecoder[Boolean]].decode(
          Form(FormField.Simple("", "1"))
        ) == Right(true)
      )
      assert(
        summon[FormDecoder[Boolean]].decode(
          Form(FormField.Simple("", "0"))
        ) == Right(false)
      )
    }

    test("decode Option[String] with Some") {
      val form = Form(FormField.Simple("test", "value"))
      val decoded = summon[FormDecoder[Option[String]]].decode(form)
      assert(decoded == Right(Some("value")))
    }

    test("decode Option[String] with empty string") {
      val form = Form(FormField.Simple("test", ""))
      val decoded = summon[FormDecoder[Option[String]]].decode(form)
      assert(decoded == Right(None))
    }

    test("decode Option[String] from binary field returns error") {
      val form =
        Form(FormField.Binary("test", Chunk.empty, MediaType.text.`plain`))
      val decoded = summon[FormDecoder[Option[String]]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode Seq[String] from comma-separated values") {
      val form = Form(FormField.Simple("test", "a,b,c"))
      val decoded = summon[FormDecoder[Seq[String]]].decode(form)
      assert(decoded == Right(Seq("a", "b", "c")))
    }

    test("decode Seq[Int] from comma-separated values") {
      val form = Form(FormField.Simple("test", "1,2,3"))
      val decoded = summon[FormDecoder[Seq[Int]]].decode(form)
      assert(decoded == Right(Seq(1, 2, 3)))
    }

    test("decode Seq[Int] fails if any element invalid") {
      val form = Form(FormField.Simple("test", "1,abc,3"))
      val decoded = summon[FormDecoder[Seq[Int]]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode empty Seq from empty string") {
      val form = Form(FormField.Simple("test", ""))
      val decoded = summon[FormDecoder[Seq[String]]].decode(form)
      assert(decoded == Right(Seq.empty))
    }

    test("decode Seq from binary field returns left") {
      val form =
        Form(FormField.Binary("test", Chunk.empty, MediaType.text.`plain`))
      val decoded = summon[FormDecoder[Seq[String]]].decode(form)
      assert(decoded.isLeft == true)
    }

    test("FormDecoder multiple fields with same key as Seq") {
      val form = Form(
        FormField.Simple("items", "value1"),
        FormField.Simple("items", "value2"),
        FormField.Simple("items", "value3")
      )
      val decoded = summon[FormDecoder[Seq[String]]].decode(form)
      assert(decoded == Right(Seq("value1", "value2", "value3")))
    }

    test("FormDecoder single field without comma as single-element Seq") {
      val form = Form(FormField.Simple("items", "alone"))
      val decoded = summon[FormDecoder[Seq[String]]].decode(form)
      assert(decoded == Right(Seq("alone")))
    }

    test("FormDecoder missing field as empty Seq (optional)") {
      val form = Form()
      val decoded = summon[FormDecoder[Seq[String]]].decode(form)
      assert(decoded == Right(Seq.empty))
    }

    test("FormDecoder missing required field fails") {
      case class RequiredField(value: String) derives FormDecoder
      val form = Form()
      val decoded = summon[FormDecoder[RequiredField]].decode(form)
      assert(decoded.isLeft)
    }

    test("FormDecoder optional field missing succeeds") {
      case class OptionalField(value: Option[String]) derives FormDecoder
      val form = Form()
      val decoded = summon[FormDecoder[OptionalField]].decode(form)
      assert(decoded == Right(OptionalField(None)))
    }

    test("FormDecoder optional field present") {
      case class OptionalField(value: Option[String]) derives FormDecoder
      val form = Form(FormField.Simple("value", "hello"))
      val decoded = summon[FormDecoder[OptionalField]].decode(form)
      assert(decoded == Right(OptionalField(Some("hello"))))
    }

    test("FormDecoder optional field empty string") {
      case class OptionalField(value: Option[String]) derives FormDecoder
      val form = Form(FormField.Simple("value", ""))
      val decoded = summon[FormDecoder[OptionalField]].decode(form)
      assert(decoded == Right(OptionalField(None)))
    }

    test("FormDecoder either left") {
      val form = Form(FormField.Simple("value", "123"))
      val decoded = summon[FormDecoder[Either[Int, String]]].decode(form)
      assert(decoded == Right(Left(123)))
    }

    test("FormDecoder either right") {
      val form = Form(FormField.Simple("value", "abc"))
      val decoded = summon[FormDecoder[Either[Int, String]]].decode(form)
      assert(decoded == Right(Right("abc")))
    }

    test("FormDecoder boolean field") {
      val form = Form(FormField.Simple("flag", "on"))
      val decoded = summon[FormDecoder[Boolean]].decode(form)
      assert(decoded == Right(true))
    }

    test("FormDecoder missing boolean field evaluates to false") {
      case class TestEntity(isActive: Boolean) derives FormDecoder
      val form = Form()
      val decoded = summon[FormDecoder[TestEntity]].decode(form)
      assert(decoded == Right(TestEntity(false)))
    }

    test("FormDecoder missing required field error message") {
      case class Required(value: String) derives FormDecoder
      val form = Form()
      val decoded = summon[FormDecoder[Required]].decode(form)
      assert(
        decoded == Left(
          Seq(DecodingError("value", "Обязательное поле"))
        )
      )
    }

    test("FormDecoder binary field with string decoder fails") {
      val form =
        Form(FormField.Binary("test", Chunk.empty, MediaType.text.`plain`))
      val decoded = summon[FormDecoder[String]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode LocalDateTime from valid string") {
      val form = Form(FormField.Simple("test", "2024-01-15T10:30"))
      val decoded = summon[FormDecoder[LocalDateTime]].decode(form)
      assert(decoded == Right(LocalDateTime.of(2024, 1, 15, 10, 30)))
    }

    test("decode LocalDateTime fallback without minutes") {
      val form = Form(FormField.Simple("test", "2024-01-15T10"))
      val decoded = summon[FormDecoder[LocalDateTime]].decode(form)
      assert(decoded == Right(LocalDateTime.of(2024, 1, 15, 10, 0)))
    }

    test("decode LocalDateTime returns error on invalid string") {
      val form = Form(FormField.Simple("test", "garbage"))
      val decoded = summon[FormDecoder[LocalDateTime]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode Option from empty form returns None") {
      val decoded = summon[FormDecoder[Option[String]]].decode(Form())
      assert(decoded == Right(None))
    }

    test("decode Boolean unrecognized value returns false") {
      val form = Form(FormField.Simple("test", "maybe"))
      val decoded = summon[FormDecoder[Boolean]].decode(form)
      assert(decoded == Right(false))
    }

    test("decode multi-field case class") {
      case class User(name: String, age: Int, active: Boolean)
          derives FormDecoder
      val form = Form(
        FormField.Simple("name", "Alice"),
        FormField.Simple("age", "30"),
        FormField.Simple("active", "true")
      )
      val decoded = summon[FormDecoder[User]].decode(form)
      assert(decoded == Right(User("Alice", 30, true)))
    }

    test("decode case class with Seq field from multiple fields") {
      case class Tags(tags: Seq[String]) derives FormDecoder
      val form = Form(
        FormField.Simple("tags", "a"),
        FormField.Simple("tags", "b"),
        FormField.Simple("tags", "c")
      )
      val decoded = summon[FormDecoder[Tags]].decode(form)
      assert(decoded == Right(Tags(Seq("a", "b", "c"))))
    }

    test(
      "decode case class with multiple missing required fields reports all errors"
    ) {
      case class TwoRequired(first: String, second: Int) derives FormDecoder
      val form = Form()
      val decoded = summon[FormDecoder[TwoRequired]].decode(form)
      assert(
        decoded == Left(
          Seq(
            DecodingError("first", "Обязательное поле"),
            DecodingError("second", "Обязательное поле")
          )
        )
      )
    }

    test("decode Option[Int] valid") {
      val form = Form(FormField.Simple("test", "42"))
      val decoded = summon[FormDecoder[Option[Int]]].decode(form)
      assert(decoded == Right(Some(42)))
    }

    test("decode Option[Int] invalid propagates error") {
      val form = Form(FormField.Simple("test", "abc"))
      val decoded = summon[FormDecoder[Option[Int]]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode negative and zero Int") {
      val negForm = Form(FormField.Simple("test", "-5"))
      assert(summon[FormDecoder[Int]].decode(negForm) == Right(-5))
      val zeroForm = Form(FormField.Simple("test", "0"))
      assert(summon[FormDecoder[Int]].decode(zeroForm) == Right(0))
    }

    test("decode Either[String, String] is left-biased") {
      val form = Form(FormField.Simple("test", "hello"))
      val decoded = summon[FormDecoder[Either[String, String]]].decode(form)
      assert(decoded == Right(Left("hello")))
    }

    test("decode Seq[Boolean] from comma-separated values") {
      val form = Form(FormField.Simple("test", "true,false,on"))
      val decoded = summon[FormDecoder[Seq[Boolean]]].decode(form)
      assert(decoded == Right(Seq(true, false, true)))
    }

    test("decode Long from simple field") {
      val form = Form(FormField.Simple("test", "9223372036854775807"))
      val decoded = summon[FormDecoder[Long]].decode(form)
      assert(decoded == Right(9223372036854775807L))
    }

    test("decode Long negative and zero") {
      val negForm = Form(FormField.Simple("test", "-100"))
      assert(summon[FormDecoder[Long]].decode(negForm) == Right(-100L))
      val zeroForm = Form(FormField.Simple("test", "0"))
      assert(summon[FormDecoder[Long]].decode(zeroForm) == Right(0L))
    }

    test("decode Long fails on non-numeric string") {
      val form = Form(FormField.Simple("test", "not-a-number"))
      val decoded = summon[FormDecoder[Long]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode Long fails on overflow") {
      val form = Form(FormField.Simple("test", "99999999999999999999"))
      val decoded = summon[FormDecoder[Long]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode UUID from valid string") {
      val uuid = UUID.randomUUID()
      val form = Form(FormField.Simple("test", uuid.toString))
      val decoded = summon[FormDecoder[UUID]].decode(form)
      assert(decoded == Right(uuid))
    }

    test("decode UUID fails on invalid string") {
      val form = Form(FormField.Simple("test", "not-a-uuid"))
      val decoded = summon[FormDecoder[UUID]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode Double from simple field") {
      val form = Form(FormField.Simple("test", "3.14"))
      val decoded = summon[FormDecoder[Double]].decode(form)
      assert(decoded == Right(3.14))
    }

    test("decode Double negative") {
      val form = Form(FormField.Simple("test", "-2.5"))
      val decoded = summon[FormDecoder[Double]].decode(form)
      assert(decoded == Right(-2.5))
    }

    test("decode Double fails on non-numeric") {
      val form = Form(FormField.Simple("test", "abc"))
      val decoded = summon[FormDecoder[Double]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode Float from simple field") {
      val form = Form(FormField.Simple("test", "2.718"))
      val decoded = summon[FormDecoder[Float]].decode(form)
      assert(decoded == Right(2.718f))
    }

    test("decode Float fails on non-numeric") {
      val form = Form(FormField.Simple("test", "xyz"))
      val decoded = summon[FormDecoder[Float]].decode(form)
      assert(decoded.isLeft)
    }

    test("decode BigDecimal from simple field") {
      val form = Form(FormField.Simple("test", "123.456"))
      val decoded = summon[FormDecoder[BigDecimal]].decode(form)
      assert(decoded == Right(BigDecimal("123.456")))
    }

    test("decode BigDecimal fails on non-numeric") {
      val form = Form(FormField.Simple("test", "not-a-number"))
      val decoded = summon[FormDecoder[BigDecimal]].decode(form)
      assert(decoded.isLeft)
    }

    test("FormDecoder map transforms value") {
      val decoder = summon[FormDecoder[Int]]
      val mapped = decoder.map(_.toString)
      val form = Form(FormField.Simple("test", "42"))
      assert(mapped.decode(form) == Right("42"))
    }

    test("FormDecoder map preserves isOptional") {
      val optionalDecoder = summon[FormDecoder[Option[String]]]
      val mapped = optionalDecoder.map(_.map(_.toUpperCase))
      assert(mapped.isOptional)
    }

    test("split decodes parameterless enum by full type name") {
      enum Color derives FormDecoder {
        case Red
        case Blue
      }
      val form = Form(FormField.Simple("color", "Red"))
      val decoded = summon[FormDecoder[Color]].decode(form)
      assert(decoded == Right(Color.Red))
    }

    test("split returns error for unknown enum variant") {
      enum Color derives FormDecoder {
        case Red
        case Blue
      }
      val form = Form(FormField.Simple("color", "Green"))
      val decoded = summon[FormDecoder[Color]].decode(form)
      assert(
        decoded == Left(
          Seq(DecodingError("", "Нет такого варианта выбора"))
        )
      )
    }

    test("split returns error on empty form for enum") {
      enum Color derives FormDecoder {
        case Red
        case Blue
      }
      val decoded = summon[FormDecoder[Color]].decode(Form())
      assert(
        decoded == Left(
          Seq(DecodingError("", "Нет такого варианта выбора"))
        )
      )
    }

    test("split returns error for non-enum sealed trait") {
      sealed trait Shape derives FormDecoder
      case class Circle(radius: Int) extends Shape derives FormDecoder
      case class Square(side: Int) extends Shape derives FormDecoder
      val form = Form(FormField.Simple("shape", "anything"))
      val decoded = summon[FormDecoder[Shape]].decode(form)
      assert(
        decoded == Left(
          Seq(DecodingError("", "Невозможно декодировать ast"))
        )
      )
    }

  }
}
