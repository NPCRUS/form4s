package demo2

import zio.*
import zio.http.*
import zio.http.template2.{form as formTag, *}
import form4s.*
import java.util.UUID
import demo.DemoHtmlForm

case class Address[F[_]](
    city: F[String],
    street: F[String]
)

case class Document[F[_]](
    typ: F[String],
    number: F[String]
)

case class RegistrationForm[F[_]](
    username: F[String],
    address: F[Address[F]],
    documents: F[Seq[Document[F]]]
)

object DemoApp extends ZIOAppDefault {
  type RegistrationFormData = RegistrationForm[[T] =>> T]

  private val addressSchema = Address(
    city = DemoHtmlForm.FormSchema.Field(
      DemoHtmlForm.FieldSchema(
        label = "Город",
        renderer = DemoHtmlForm.stringRenderable,
        placeholderAttr = "Введите город",
        typeAttr = "text"
      )
    ),
    street = DemoHtmlForm.FormSchema.Field(
      DemoHtmlForm.FieldSchema(
        label = "Улица",
        renderer = DemoHtmlForm.stringRenderable,
        placeholderAttr = "Введите улицу",
        typeAttr = "text"
      )
    )
  )

  private val documentSchema = Document(
    typ = DemoHtmlForm.FormSchema.Field(
      DemoHtmlForm.FieldSchema(
        label = "Тип документа",
        renderer = DemoHtmlForm.stringRenderable,
        placeholderAttr = "Например, паспорт",
        typeAttr = "text"
      )
    ),
    number = DemoHtmlForm.FormSchema.Field(
      DemoHtmlForm.FieldSchema(
        label = "Номер",
        renderer = DemoHtmlForm.stringRenderable,
        placeholderAttr = "Введите номер",
        typeAttr = "text"
      )
    )
  )

  given RegistrationForm[DemoHtmlForm.FormSchema] = RegistrationForm(
    username = DemoHtmlForm.FormSchema.Field(
      DemoHtmlForm.FieldSchema(
        label = "Имя пользователя",
        renderer = DemoHtmlForm.stringRenderable,
        placeholderAttr = "Введите имя",
        typeAttr = "text",
        validator =
          Validator.compose(Validator.nonEmpty, Validator.minLength(3)).toZIO
      )
    ),
    address = DemoHtmlForm.FormSchema.SubForm(
      label = "Адрес",
      form = addressSchema
    ),
    documents = DemoHtmlForm.FormSchema.RepeatedSubForm(
      label = "Документы",
      form = documentSchema
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
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Город")
                ),
                dd(
                  `class` := "text-sm text-gray-900",
                  text(data.address.city)
                ),
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Улица")
                ),
                dd(
                  `class` := "text-sm text-gray-900",
                  text(data.address.street)
                ),
                dt(
                  `class` := "text-sm font-medium text-gray-500",
                  text("Документы")
                ),
                data.documents.zipWithIndex.map { (doc, i) =>
                  dd(
                    `class` := "text-sm text-gray-900",
                    text(s"${i + 1}. ${doc.typ} — ${doc.number}")
                  )
                }
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
      val form = DemoHtmlForm.draw[RegistrationForm](
        None,
        Map.empty
      )
      renderPage(form)
    },
    Method.POST / "" -> handler { (req: Request) =>
      req.body.asURLEncodedForm.flatMap { fd =>
        DemoHtmlForm
          .decodeAndValidate[RegistrationForm](fd)
          .fold(
            incomplete =>
              renderPage(
                DemoHtmlForm.draw[RegistrationForm](
                  incomplete.oldForm,
                  incomplete.errors
                )
              ),
            renderSuccess
          )
      }
    }
  )

  def run: ZIO[Any, Throwable, Nothing] =
    Server.serve(routes.sandbox).provide(Server.defaultWithPort(8080))
}
