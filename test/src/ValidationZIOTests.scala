package form4s

import utest.*
import zio.*
import zio.blocks.schema.Schema
import zio.http.{Form, FormField}

case class Shop(name: String, isMain: Boolean)
object Shop extends AutoFields[Shop]:
  given Schema[Shop] = Schema.derived

case class Org(name: String, age: Option[Int], shops: Seq[Shop])
object Org extends AutoFields[Org]:
  given Schema[Org] = Schema.derived

case class EmailForm(email: String)
object EmailForm extends AutoFields[EmailForm]:
  given Schema[EmailForm] = Schema.derived

trait EmailBlocklist:
  def contains(email: String): UIO[Boolean]

object EmailBlocklist:
  val live: ULayer[EmailBlocklist] =
    ZLayer.succeed(new EmailBlocklist {
      def contains(email: String): UIO[Boolean] =
        ZIO.succeed(email == "x@spam.com")
    })

object ValidationZIOTests extends TestSuite:

  def run[E, A](z: ZIO[Any, E, A]): Either[E, A] =
    Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.run(z.either)(using Trace.empty, u).getOrThrow()
    }

  given FormDecoder[Org] = FormDecoder.derived
  given FormDecoder[Shop] = FormDecoder.derived
  given FormDecoder[EmailForm] = FormDecoder.derived

  val effectfulNonEmpty: ValidatorZIO[String, Any] = in =>
    ZIO.succeed(if in.isEmpty then Seq("Empty!") else Seq.empty)

  val effectfulShopsNonEmpty: ValidatorZIO[Org, Any] = o =>
    ZIO.succeed(
      if o.shops.isEmpty then Seq("Need at least one shop") else Seq.empty
    )

  val notBlocklisted: ValidatorZIO[String, EmailBlocklist] = in =>
    ZIO
      .serviceWithZIO[EmailBlocklist](_.contains(in))
      .map(if _ then Seq("Email is blocklisted") else Seq.empty)

  given Validation[Org, Any] = Validation(rule(Org.name, effectfulNonEmpty))

  val tests = Tests {

    test("ValidatorZIO.option only validates Some") {
      val v = effectfulNonEmpty.option
      assert(run(v.validate(Some("x"))).toOption.get.isEmpty)
      assert(run(v.validate(None)).toOption.get.isEmpty)
      assert(run(v.validate(Some(""))).toOption.get == Seq("Empty!"))
    }

    test("ValidatorZIO.++ concatenates") {
      val v = effectfulNonEmpty ++ Validator.minLength(3).toZIO
      val empty = run(v.validate("")).toOption.get
      assert(empty.contains("Empty!"))
      assert(empty.contains("Минимум 3 символов"))
      assert(run(v.validate("ab")).toOption.get == Seq("Минимум 3 символов"))
    }

    test("rule accepts an effectful validator") {
      val v = Validation(rule(Org.name, effectfulNonEmpty))
      assert(
        run(v.run(Org("", None, Seq.empty))).toOption.get ==
          Map("name" -> Seq("Empty!"))
      )
    }

    test("rule auto-lifts a pure validator") {
      val v = Validation(rule(Org.name, Validator.nonEmpty))
      assert(
        run(v.run(Org("", None, Seq.empty))).toOption.get ==
          Map("name" -> Seq("Поле должно быть заполнено"))
      )
    }

    test("whole with effectful validator keyed at a section path") {
      val v = Validation(whole(effectfulShopsNonEmpty, "shops"))
      assert(
        run(v.run(Org("acme", None, Seq.empty))).toOption.get ==
          Map("shops" -> Seq("Need at least one shop"))
      )
    }

    test("whole auto-lifts a pure validator") {
      val v = Validation(
        whole(
          Validator.custom[Org]("Need at least one shop")(_.shops.nonEmpty),
          "shops"
        )
      )
      assert(
        run(v.run(Org("acme", None, Seq.empty))).toOption.get ==
          Map("shops" -> Seq("Need at least one shop"))
      )
    }

    test("ruleEach with effectful sub-validation produces indexed keys") {
      val v = Validation(
        ruleEach(Org.shops, Validation(rule(Shop.name, effectfulNonEmpty)))
      )
      val org = Org("acme", None, Seq(Shop("", false), Shop("ok", true)))
      assert(
        run(v.run(org)).toOption.get ==
          Map("shops.0.name" -> Seq("Empty!"))
      )
    }

    test("Validation.++ mixes pure and effectful rules") {
      val v =
        Validation(rule(Org.name, effectfulNonEmpty)) ++
          Validation(rule(Org.age, Validator.required[Int]))
      val errs = run(v.run(Org("", None, Seq.empty))).toOption.get
      assert(errs("name") == Seq("Empty!"))
      assert(errs("age") == Seq("Обязательное поле"))
    }

    test("decodeAndValidate succeeds") {
      val form = Form(FormField.Simple("name", "acme"))
      val result = run(decodeAndValidate[Org](form))
      assert(result.isRight)
      assert(result.toOption.get.name == "acme")
    }

    test("decodeAndValidate fails validation with old form") {
      val form = Form(FormField.Simple("name", ""))
      val result = run(decodeAndValidate[Org](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("name") == Seq("Empty!"))
      assert(incomplete.oldForm.isDefined)
      assert(incomplete.oldForm.get.name == "")
    }

    test("decodeAndValidate fails decode without old form") {
      val form = Form()
      val result = run(decodeAndValidate[Org](form))
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("name") == Seq("Обязательное поле"))
      assert(incomplete.oldForm.isEmpty)
    }

    test("validator captures a ZIO environment service") {
      val v = Validation(
        rule(EmailForm.email, Validator.isEmail), // pure, auto-widens
        rule(EmailForm.email, notBlocklisted) // requires EmailBlocklist
      )

      val ok =
        run(v.run(EmailForm("a@b.c")).provideLayer(EmailBlocklist.live))
      assert(ok.toOption.get.isEmpty)

      val bad =
        run(v.run(EmailForm("x@spam.com")).provideLayer(EmailBlocklist.live))
      assert(bad.toOption.get("email") == Seq("Email is blocklisted"))
    }

    test("decodeAndValidate threads the ZIO environment") {
      given Validation[EmailForm, EmailBlocklist] = Validation(
        rule(EmailForm.email, notBlocklisted)
      )

      val form = Form(FormField.Simple("email", "x@spam.com"))
      val result =
        run(
          decodeAndValidate[EmailForm](form).provideLayer(
            EmailBlocklist.live
          )
        )
      assert(result.isLeft)
      val incomplete = result.swap.toOption.get
      assert(incomplete.errors("email") == Seq("Email is blocklisted"))
    }
  }
