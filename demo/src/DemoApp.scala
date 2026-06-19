package demo

import zio.*
import zio.http.*
import zio.http.template2.{form as formTag, *}
import form.Validator
import util.{FormDecoder, DecodingError}

enum Role:
  case Student, Developer, Designer, Manager, Other

object Role:
  given FormDecoder[Role] = new FormDecoder[Role]:
    def decode(input: zio.http.Form): Either[Seq[DecodingError], Role] =
      summon[FormDecoder[String]].decode(input).flatMap { str =>
        Role.values
          .find(_.toString == str)
          .toRight(Seq(DecodingError("", "Неизвестная роль")))
      }

case class RegistrationForm[F[_]](
    username: F[String],
    email: F[String],
    age: F[Int],
    role: F[Role],
    bio: F[Option[String]],
    agree: F[Boolean]
)

object DemoApp extends ZIOAppDefault {
  type RegistrationFormData = RegistrationForm[[T] =>> T]

  given RegistrationForm[DemoHtmlForm.FieldSchema] = RegistrationForm(
    username = DemoHtmlForm.FieldSchema(
      label = "Имя пользователя",
      renderer = DemoHtmlForm.stringRenderable,
      placeholderAttr = "Введите имя",
      typeAttr = "text",
      validator = Validator.compose(Validator.nonEmpty, Validator.minLength(3))
    ),
    email = DemoHtmlForm.FieldSchema(
      label = "Email",
      renderer = DemoHtmlForm.stringRenderable,
      placeholderAttr = "example@mail.com",
      typeAttr = "email",
      validator = Validator.compose(Validator.nonEmpty, Validator.isEmail)
    ),
    age = DemoHtmlForm.FieldSchema(
      label = "Возраст",
      renderer = DemoHtmlForm.intRenderable,
      placeholderAttr = "Введите возраст",
      typeAttr = "number",
      validator = Validator.compose(Validator.min(1), Validator.max(150))
    ),
    role = DemoHtmlForm.FieldSchema(
      label = "Роль",
      renderer = DemoHtmlForm.selectRenderable[Role](_.toString),
      placeholderAttr = "",
      typeAttr = "select",
      options = Role.values.map(_.toString).toSeq
    ),
    bio = DemoHtmlForm.FieldSchema(
      label = "О себе",
      renderer = DemoHtmlForm.stringRenderable.optional,
      placeholderAttr = "Расскажите о себе",
      typeAttr = "text"
    ),
    agree = DemoHtmlForm.FieldSchema(
      label = "Я согласен с условиями",
      renderer = DemoHtmlForm.boolRenderable,
      placeholderAttr = "",
      typeAttr = "checkbox",
      validator = Validator.requiredTrue
    )
  )

  private def renderPage(formContent: Dom): Response =
    Response.html(
      html(
        head(
          meta(charset := "utf-8"),
          meta(
            name := "viewport",
            content := "width=device-width, initial-scale=1"
          ),
          script.externalJs("https://cdn.tailwindcss.com")
        ),
        body(
          `class` := "bg-gray-100 min-h-screen flex items-center justify-center py-12",
          div(
            `class` := "bg-white p-8 rounded-lg shadow-md w-full max-w-md",
            h1(
              `class` := "text-2xl font-bold text-gray-900 mb-6",
              text("Регистрация")
            ),
            formTag(
              method := "post",
              action := "/",
              formContent,
              button(
                `type` := "submit",
                `class` := "w-full bg-indigo-600 text-white py-2 px-4 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 mt-2",
                text("Зарегистрироваться")
              )
            )
          )
        )
      )
    )

  private def renderSuccess(data: RegistrationFormData): Response =
    Response.html(
      html(
        head(
          meta(charset := "utf-8"),
          meta(
            name := "viewport",
            content := "width=device-width, initial-scale=1"
          ),
          script.externalJs("https://cdn.tailwindcss.com")
        ),
        body(
          `class` := "bg-gray-100 min-h-screen flex items-center justify-center py-12",
          div(
            `class` := "bg-white p-8 rounded-lg shadow-md w-full max-w-md",
            div(
              `class` := "bg-green-50 border border-green-200 rounded-lg p-6",
              h2(
                `class` := "text-xl font-bold text-green-800 mb-4",
                text("Регистрация успешна!")
              ),
              dl(
                `class` := "space-y-2",
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Имя пользователя")
                ),
                dd(`class` := "text-sm text-gray-900", text(data.username)),
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Email")
                ),
                dd(`class` := "text-sm text-gray-900", text(data.email)),
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Возраст")
                ),
                dd(`class` := "text-sm text-gray-900", text(data.age.toString)),
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Роль")
                ),
                dd(
                  `class` := "text-sm text-gray-900",
                  text(data.role.toString)
                ),
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("О себе")
                ),
                dd(
                  `class` := "text-sm text-gray-900",
                  text(data.bio.getOrElse("—"))
                )
              ),
              a(
                href := "/",
                `class` := "mt-4 inline-block text-indigo-600 hover:text-indigo-500 text-sm",
                text("← Вернуться к форме")
              )
            )
          )
        )
      )
    )

  val routes = Routes(
    Method.GET / "" -> handler {
      renderPage(DemoHtmlForm.draw[RegistrationForm](None, Map.empty))
    },
    Method.POST / "" -> handler { (req: Request) =>
      req.body.asURLEncodedForm.map { fd =>
        FormDecoder.decode[RegistrationFormData](fd) match {
          case Left(errors) =>
            renderPage(
              DemoHtmlForm.draw[RegistrationForm](
                None,
                errors.groupMap(_.field)(_.message)
              )
            )
          case Right(decoded) =>
            val validationErrors =
              DemoHtmlForm
                .validate[RegistrationForm](decoded)
                .filter(_._2.nonEmpty)
            if (validationErrors.nonEmpty)
              renderPage(
                DemoHtmlForm.draw[RegistrationForm](
                  Some(decoded),
                  validationErrors
                )
              )
            else renderSuccess(decoded)
        }
      }
    }
  )

  def run: ZIO[Any, Throwable, Nothing] =
    Server.serve(routes.sandbox).provide(Server.defaultWithPort(8080))
}
