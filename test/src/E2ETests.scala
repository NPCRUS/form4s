package form4s

import utest.*
import zio.*
import zio.http.*
import zio.http.template2.{form as formTag, *}
import com.microsoft.playwright.{Playwright, Browser, BrowserType}
import java.util.concurrent.atomic.AtomicReference

case class TestUserForm[F[_]](
    username: F[String],
    age: F[Int]
)

object E2ETests extends TestSuite {
  val port = 9876

  type TestUserFormData = TestUserForm[[T] =>> T]
  val lastDecodeResult =
    new AtomicReference[Either[Map[String, Seq[String]], TestUserFormData]]

  given TestUserForm[HtmlForm.FieldSchema] = TestUserForm(
    username = HtmlForm.FieldSchema(
      label = "Username",
      renderer = HtmlForm.stringRenderable,
      placeholderAttr = "Enter username",
      typeAttr = "text",
      validator = Validator.nonEmpty.toZIO
    ),
    age = HtmlForm.FieldSchema(
      label = "Age",
      renderer = HtmlForm.intRenderable,
      placeholderAttr = "Enter age",
      typeAttr = "number"
    )
  )

  val routes = Routes(
    Method.GET / "" -> handler {
      Response.html(
        html(
          head(meta(charset := "utf-8")),
          body(
            formTag(
              method := "post",
              action := "/",
              HtmlForm.draw[TestUserForm](None, Map.empty),
              button(`type` := "submit", text("Submit"))
            )
          )
        )
      )
    },
    Method.POST / "" -> handler { (req: Request) =>
      req.body.asURLEncodedForm.flatMap { formData =>
        HtmlForm.decodeAndValidate[TestUserForm](formData).map { result =>
          lastDecodeResult.set(result)
          result match {
            case Right(user) =>
              Response.html(
                html(
                  head(meta(charset := "utf-8")),
                  body(
                    div(
                      id := "result",
                      text(s"Success: ${user.username}, ${user.age}")
                    )
                  )
                )
              )
            case Left(errors) =>
              val errorDivs = errors.toSeq.flatMap { case (field, msgs) =>
                msgs.map(m => p(`class` := "form-error", text(s"$field: $m")))
              }
              Response.html(
                html(
                  head(meta(charset := "utf-8")),
                  body(
                    div(id := "errors", errorDivs),
                    formTag(
                      method := "post",
                      action := "/",
                      HtmlForm.draw[TestUserForm](None, errors),
                      button(`type` := "submit", text("Submit"))
                    )
                  )
                )
              )
          }
        }
      }
    }
  )

  lazy val serverFiber = {
    val fiber = Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.fork(
        Server.serve(routes.sandbox).provide(Server.defaultWithPort(port))
      )(using Trace.empty, u)
    }
    Thread.sleep(2000)
    fiber
  }

  lazy val serverStarted: Boolean = { serverFiber; true }

  lazy val pw: Playwright = Playwright.create()
  lazy val browser: Browser =
    pw.chromium().launch(new BrowserType.LaunchOptions().setHeadless(true))

  override def utestAfterAll(): Unit = {
    if (browser != null) browser.close()
    if (pw != null) pw.close()
    Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe
        .run(serverFiber.interrupt)(using Trace.empty, u)
    }
  }

  val tests = Tests {
    test("renders form with inputs") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      assert(page.locator("input[name=username]").count() == 1)
      assert(page.locator("input[name=age]").count() == 1)
      assert(page.locator("button[type=submit]").count() == 1)
      page.close()
    }

    test("submit valid form shows success") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      page.fill("input[name=username]", "Alice")
      page.fill("input[name=age]", "30")
      page.click("button[type=submit]")
      page.waitForLoadState()
      val bodyText = page.textContent("body")
      assert(bodyText.contains("Alice"))
      assert(bodyText.contains("30"))
      val result = lastDecodeResult.get()
      assert(result.isRight)
      val user = result.toOption.get
      assert(user.username == "Alice")
      assert(user.age == 30)
      page.close()
    }

    test("submit with empty username shows validation error") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      page.fill("input[name=username]", "")
      page.fill("input[name=age]", "30")
      page.click("button[type=submit]")
      page.waitForLoadState()
      val bodyText = page.textContent("body")
      assert(bodyText.contains("Пустое поле запрещено"))
      val result = lastDecodeResult.get()
      val errors = result.swap.getOrElse(Map.empty)
      assert(errors("username") == Seq("Пустое поле запрещено"))
      assert(!errors.contains("age"))
      page.close()
    }

    test("submit with invalid age shows decode error") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      page.fill("input[name=username]", "Alice")
      page.fill("input[name=age]", "")
      page.click("button[type=submit]")
      page.waitForLoadState()
      val bodyText = page.textContent("body")
      assert(bodyText.contains("Невозможно преобразовать в число"))
      val result = lastDecodeResult.get()
      val errors = result.swap.getOrElse(Map.empty)
      assert(errors("age") == Seq("Невозможно преобразовать в число"))
      page.close()
    }
  }
}
