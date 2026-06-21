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
      assert(Validator.nonEmpty.validate("") == Seq("Пустое поле запрещено"))
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
      assert(Validator.maxLength(3).validate("abcd") == Seq("Максимум 3 символов"))
    }

    test("isEmail valid") {
      assert(Validator.isEmail.validate("a@b").isEmpty)
    }

    test("isEmail invalid") {
      assert(Validator.isEmail.validate("no-at") == Seq("Некорректный email"))
    }

    test("matches valid") {
      assert(Validator.matches("""\d+""".r).validate("123").isEmpty)
    }

    test("matches invalid") {
      assert(Validator.matches("""\d+""".r).validate("abc") == Seq("Неверный формат"))
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
      assert(Validator.requiredTrue.validate(false) == Seq("Требуется подтверждение"))
    }

    test("compose empty") {
      assert(Validator.compose[String]().validate("x").isEmpty)
    }

    test("compose single passing") {
      assert(Validator.compose(Validator.nonEmpty).validate("x").isEmpty)
    }

    test("compose single failing") {
      assert(Validator.compose(Validator.nonEmpty).validate("") == Seq("Пустое поле запрещено"))
    }

    test("compose multiple failures") {
      val errors = Validator.compose(Validator.minLength(5), Validator.minLength(10)).validate("x")
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
  }
}
