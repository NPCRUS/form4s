package form4s

import utest._

object ValidatorTests extends TestSuite {
  val tests = Tests {
    test("empty") {
      assert(Validator.empty[Int].validate(5).isEmpty)
      assert(Validator.empty[String].validate("").isEmpty)
    }

    test("nonEmpty valid") {
      assert(Validator.nonEmpty.validate("a").isEmpty)
    }

    test("nonEmpty invalid") {
      assert(Validator.nonEmpty.validate("") == Seq("Поле должно быть заполнено"))
    }

    test("required valid") {
      assert(Validator.required.validate(Some(1)).isEmpty)
    }

    test("required invalid") {
      assert(Validator.required.validate(None) == Seq("Обязательное поле"))
    }

    test("minLength valid") {
      assert(Validator.minLength(3).validate("abc").isEmpty)
    }

    test("minLength invalid") {
      assert(Validator.minLength(3).validate("ab") == Seq("Минимум 3 символов"))
    }

    test("maxLength valid") {
      assert(Validator.maxLength(3).validate("abc").isEmpty)
    }

    test("maxLength invalid") {
      assert(
        Validator.maxLength(3).validate("abcd") == Seq("Максимум 3 символов")
      )
    }

    test("isEmail valid") {
      assert(Validator.isEmail.validate("a@b").isEmpty)
    }

    test("isEmail valid with dots and plus") {
      assert(Validator.isEmail.validate("john.doe+tag@example.com").isEmpty)
    }

    test("isEmail invalid no at") {
      assert(Validator.isEmail.validate("no-at") == Seq("Некорректный email"))
    }

    test("isEmail invalid space") {
      assert(
        Validator.isEmail.validate("a b@example.com") == Seq(
          "Некорректный email"
        )
      )
    }

    test("isEmail invalid leading at") {
      assert(
        Validator.isEmail.validate("@example.com") == Seq("Некорректный email")
      )
    }

    test("isEmail invalid trailing junk") {
      assert(Validator.isEmail.validate("a@b c") == Seq("Некорректный email"))
    }

    test("matches valid") {
      assert(Validator.matches("""\d+""".r).validate("123").isEmpty)
    }

    test("matches invalid") {
      assert(
        Validator.matches("""\d+""".r).validate("abc") == Seq("Неверный формат")
      )
    }

    test("min valid") {
      assert(Validator.min(5).validate(5).isEmpty)
      assert(Validator.min(5).validate(6).isEmpty)
    }

    test("min invalid") {
      assert(Validator.min(5).validate(4) == Seq("Минимум 5"))
    }

    test("max valid") {
      assert(Validator.max(5).validate(5).isEmpty)
      assert(Validator.max(5).validate(4).isEmpty)
    }

    test("max invalid") {
      assert(Validator.max(5).validate(6) == Seq("Максимум 5"))
    }

    test("requiredTrue valid") {
      assert(Validator.requiredTrue.validate(true).isEmpty)
    }

    test("requiredTrue invalid") {
      assert(
        Validator.requiredTrue.validate(false) == Seq("Требуется подтверждение")
      )
    }

    test("compose empty") {
      assert(Validator.compose[String]().validate("x").isEmpty)
    }

    test("compose single passing") {
      assert(Validator.compose(Validator.nonEmpty).validate("x").isEmpty)
    }

    test("compose single failing") {
      assert(
        Validator.compose(Validator.nonEmpty).validate("") == Seq(
          "Поле должно быть заполнено"
        )
      )
    }

    test("compose multiple failures") {
      val errors = Validator
        .compose(Validator.minLength(5), Validator.minLength(10))
        .validate("x")
      assert(errors == Seq("Минимум 5 символов", "Минимум 10 символов"))
    }

    test("isPhone valid +79184499126") {
      val errors = Validator.isPhone.validate("+79184499126")
      assert(errors.isEmpty)
    }

    test("isPhone valid 10 digits") {
      val errors = Validator.isPhone.validate("+1234567890")
      assert(errors.isEmpty)
    }

    test("isPhone valid 15 digits") {
      val errors = Validator.isPhone.validate("+123456789012345")
      assert(errors.isEmpty)
    }

    test("isPhone invalid no plus") {
      val errors = Validator.isPhone.validate("79184499126")
      assert(errors == Seq("Некорректный номер телефона"))
    }

    test("isPhone invalid too short") {
      val errors = Validator.isPhone.validate("+123456789")
      assert(errors == Seq("Некорректный номер телефона"))
    }

    test("isPhone invalid letters") {
      val errors = Validator.isPhone.validate("+7918ABC9126")
      assert(errors == Seq("Некорректный номер телефона"))
    }

    test("isPhone invalid empty") {
      val errors = Validator.isPhone.validate("")
      assert(errors == Seq("Некорректный номер телефона"))
    }

    test("positive valid") {
      assert(Validator.positive.validate(1).isEmpty)
      assert(Validator.positive.validate(100).isEmpty)
    }

    test("positive invalid zero") {
      assert(
        Validator.positive.validate(0) == Seq(
          "Значение должно быть положительным"
        )
      )
    }

    test("positive invalid negative") {
      assert(
        Validator.positive.validate(-5) == Seq(
          "Значение должно быть положительным"
        )
      )
    }

    test("isUrl valid http") {
      assert(Validator.isUrl.validate("http://example.com").isEmpty)
    }

    test("isUrl valid https") {
      assert(Validator.isUrl.validate("https://example.com/path?a=1").isEmpty)
    }

    test("isUrl invalid no protocol") {
      assert(Validator.isUrl.validate("example.com") == Seq("Некорректный URL"))
    }

    test("isUrl invalid empty") {
      assert(Validator.isUrl.validate("") == Seq("Некорректный URL"))
    }

    test("custom valid") {
      val even = Validator.custom[Int]("Должно быть чётным")(_ % 2 == 0)
      assert(even.validate(4).isEmpty)
    }

    test("custom invalid") {
      val even = Validator.custom[Int]("Должно быть чётным")(_ % 2 == 0)
      assert(even.validate(3) == Seq("Должно быть чётным"))
    }

    test("option None produces no errors") {
      assert(Validator.minLength(3).option.validate(None).isEmpty)
    }

    test("option Some passing inner validator") {
      assert(Validator.minLength(3).option.validate(Some("abcd")).isEmpty)
    }

    test("option Some failing inner validator") {
      val errors = Validator.minLength(3).option.validate(Some("ab"))
      assert(errors == Seq("Минимум 3 символов"))
    }

    test("option composed with required passes Some") {
      val inner: Validator[Option[String]] = Validator.minLength(3).option
      val requiredString: Validator[Option[String]] =
        Validator.required.asInstanceOf[Validator[Option[String]]]
      val v = Validator.compose(inner, requiredString)
      assert(v.validate(Some("abcd")).isEmpty)
    }

    test("option composed with required fails on None") {
      val inner: Validator[Option[String]] = Validator.minLength(3).option
      val requiredString: Validator[Option[String]] =
        Validator.required.asInstanceOf[Validator[Option[String]]]
      val v = Validator.compose(inner, requiredString)
      assert(v.validate(None) == Seq("Обязательное поле"))
    }
  }
}
