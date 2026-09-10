package form4s

import utest.*
import zio.*
import zio.blocks.schema.Schema
import zio.http.*
import zio.http.template2.{form as formTag, *}
import com.microsoft.playwright.{Playwright, Browser, BrowserType}
import java.util.concurrent.atomic.AtomicReference

case class TestAddress(city: String, street: String)
object TestAddress extends AutoFields[TestAddress]:
  given Schema[TestAddress] = Schema.derived

case class TestUser(username: String, age: Int, address: TestAddress)
object TestUser extends AutoFields[TestUser]:
  given Schema[TestUser] = Schema.derived

object E2ETests extends TestSuite:
  val port = 9876

  val lastDecodeResult =
    new AtomicReference[Either[Map[String, Seq[String]], TestUser]]

  given FormDecoder[TestUser] = FormDecoder.derived

  given Validation[TestUser, Any] = Validation(
    rule(TestUser.username, Validator.nonEmpty)
  )

  def textInput(ctx: RenderContext[?]): List[Dom] =
    List(input(name := ctx.path, `type` := "text"))

  def numberInput(ctx: RenderContext[?]): List[Dom] =
    List(input(name := ctx.path, `type` := "number"))

  val addressForm: FormRenderer[TestAddress, List[Dom]] =
    draw(
      drawField(TestAddress.city)(ctx => textInput(ctx)),
      drawField(TestAddress.street)(ctx => textInput(ctx))
    )(_ ++ _)

  val userForm: FormRenderer[TestUser, List[Dom]] =
    draw(
      drawField(TestUser.username)(ctx => textInput(ctx)),
      drawField(TestUser.age)(ctx => numberInput(ctx)),
      drawField(TestUser.address)(ctx => addressForm(ctx))
    )(_ ++ _)

  def renderFormContent(
      old: Option[TestUser],
      errors: Map[String, Seq[String]]
  ): List[Dom] = renderForm(userForm, old, errors)

  val routes = Routes(
    Method.GET / "" -> handler {
      val content = renderFormContent(None, Map.empty)
      Response.html(
        html(
          head(meta(charset := "utf-8")),
          body(
            formTag(
              method := "post",
              action := "/",
              noValidate,
              content,
              button(`type` := "submit", text("Submit"))
            )
          )
        )
      )
    },
    Method.POST / "" -> handler { (req: Request) =>
      req.body.asURLEncodedForm.flatMap { formData =>
        decodeAndValidate[TestUser](formData).fold(
          incomplete => {
            lastDecodeResult.set(Left(incomplete.errors))
            val errorDivs =
              incomplete.errors.toSeq.flatMap { case (field, msgs) =>
                msgs.map(m => p(`class` := "form-error", text(s"$field: $m")))
              }
            val content =
              renderFormContent(incomplete.oldForm, incomplete.errors)
            Response.html(
              html(
                head(meta(charset := "utf-8")),
                body(
                  div(id := "errors", errorDivs),
                  formTag(
                    method := "post",
                    action := "/",
                    noValidate,
                    content,
                    button(`type` := "submit", text("Submit"))
                  )
                )
              )
            )
          },
          user => {
            lastDecodeResult.set(Right(user))
            Response.html(
              html(
                head(meta(charset := "utf-8")),
                body(
                  div(
                    id := "result",
                    text(
                      s"Success: ${user.username}, ${user.age}, ${user.address.city}, ${user.address.street}"
                    )
                  )
                )
              )
            )
          }
        )
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
      assert(page.locator("input[name='address.city']").count() == 1)
      assert(page.locator("input[name='address.street']").count() == 1)
      assert(page.locator("button[type=submit]").count() == 1)
      page.close()
    }

    test("submit valid form shows success") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      page.fill("input[name=username]", "Alice")
      page.fill("input[name=age]", "30")
      page.fill("input[name='address.city']", "NYC")
      page.fill("input[name='address.street']", "Main St")
      page.click("button[type=submit]")
      page.waitForLoadState()
      val bodyText = page.textContent("body")
      assert(bodyText.contains("Alice"))
      assert(bodyText.contains("30"))
      assert(bodyText.contains("NYC"))
      assert(bodyText.contains("Main St"))
      val result = lastDecodeResult.get()
      assert(result.isRight)
      val user = result.toOption.get
      assert(user.username == "Alice")
      assert(user.age == 30)
      assert(user.address.city == "NYC")
      assert(user.address.street == "Main St")
      page.close()
    }

    test("submit with empty username shows validation error") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      page.fill("input[name=username]", "")
      page.fill("input[name=age]", "30")
      page.fill("input[name='address.city']", "NYC")
      page.fill("input[name='address.street']", "Main St")
      page.click("button[type=submit]")
      page.waitForLoadState()
      val bodyText = page.textContent("body")
      assert(bodyText.contains("Поле должно быть заполнено"))
      val result = lastDecodeResult.get()
      val errors = result.swap.getOrElse(Map.empty)
      assert(errors("username") == Seq("Поле должно быть заполнено"))
      assert(!errors.contains("age"))
      assert(!errors.contains("address.city"))
      assert(!errors.contains("address.street"))
      page.close()
    }

    test("submit with invalid age shows decode error") {
      assert(serverStarted)
      val page = browser.newPage()
      page.navigate(s"http://localhost:$port/")
      page.fill("input[name=username]", "Alice")
      page.fill("input[name=age]", "")
      page.fill("input[name='address.city']", "NYC")
      page.fill("input[name='address.street']", "Main St")
      page.click("button[type=submit]")
      page.waitForLoadState()
      val bodyText = page.textContent("body")
      assert(bodyText.contains("Невозможно преобразовать в число"))
      val result = lastDecodeResult.get()
      val errors = result.swap.getOrElse(Map.empty)
      assert(errors("age") == Seq("Невозможно преобразовать в число"))
      assert(!errors.contains("address.city"))
      assert(!errors.contains("address.street"))
      page.close()
    }
  }
