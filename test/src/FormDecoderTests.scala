package util

import utest._
import zio.http.{Form, FormField, MediaType}
import zio.Chunk
import java.time.LocalDateTime

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
          Seq(DecodingError("value", "Required field is missing"))
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

    test("decode LocalDateTime throws on invalid string") {
      val form = Form(FormField.Simple("test", "garbage"))
      intercept[Exception] {
        summon[FormDecoder[LocalDateTime]].decode(form)
      }
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
            DecodingError("first", "Required field is missing"),
            DecodingError("second", "Required field is missing")
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

  }
}
