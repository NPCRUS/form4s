package demo

import form4s.*
import zio.*
import zio.http.*
import zio.http.template2.{form as formTag, *}

object DemoApp extends ZIOAppDefault:

  given FormDecoder[Registration] = FormDecoder.derived

  given Validation[Registration] = Validation(
    rule(Registration.username, Validator.nonEmpty ++ Validator.minLength(3)),
    rule(Registration.email, Validator.isEmail),
    rule(
      Registration.age,
      Validator.custom[Option[Int]]("Возраст должен быть от 1 до 150")(
        _.forall(a => a >= 1 && a <= 150)
      )
    ),
    rule(
      Registration.score,
      Validator.custom[Long]("Баллы должны быть неотрицательными")(_ >= 0)
    ),
    rule(Registration.agree, Validator.requiredTrue),
    rule(Registration.address / Address.city, Validator.nonEmpty),
    rule(Registration.address / Address.street, Validator.nonEmpty),
    rule(Registration.documents, Validator.nonEmptySeq),
    whole(
      Validator.custom[Registration](
        "Имя пользователя не должно совпадать с email"
      )(r => r.username != r.email)
    )
  )

  private def bootstrapHead: Dom =
    head(
      meta(charset := "utf-8"),
      meta(
        name := "viewport",
        content := "width=device-width, initial-scale=1"
      ),
      link(
        href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
        rel := "stylesheet"
      )
    )

  private def bootstrapScript: Dom =
    script(
      src := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"
    )

  private def page(content: Seq[Dom]): Response =
    Response.html(
      html(
        bootstrapHead,
        body(
          `class` := "bg-light min-vh-100 py-4",
          div(
            `class` := "container",
            custom("style") := "max-width: 720px;",
            content
          )
        ),
        bootstrapScript
      )
    )

  private def renderPage(
      old: Option[Registration],
      errors: Map[String, Seq[String]]
  ): Response =
    val rootErrors = errors.getOrElse("", Seq.empty)
    val formContent = renderForm(Views.registrationForm, old, errors)
    val content: Seq[Dom] =
      Seq(h1(`class` := "h3 mb-4", text("Регистрация"))) ++
        rootErrors.map(e => div(`class` := "alert alert-danger", text(e))) :+
        formTag(
          method := "post",
          action := "/",
          noValidate,
          `class` := "card card-body",
          formContent,
          button(
            `type` := "submit",
            `class` := "btn btn-primary w-100",
            text("Зарегистрироваться")
          )
        )
    page(content)

  private def row(label: String, value: String): Dom =
    div(
      `class` := "mb-1",
      span(`class` := "text-muted", text(s"$label: ")),
      text(value)
    )

  private def renderSuccess(r: Registration): Response =
    val content: Seq[Dom] = Seq(
      div(
        `class` := "alert alert-success",
        h2(`class` := "h4", text("Регистрация успешна!")),
        row("Имя пользователя", r.username),
        row("Email", r.email),
        row("Возраст", r.age.map(_.toString).getOrElse("—")),
        row("Баллы", r.score.toString),
        row("Реферальный ID", r.referralId.toString),
        row("Роль", Role.label(r.role)),
        row("О себе", r.bio.getOrElse("—")),
        row("Согласие", if r.agree then "Да" else "Нет"),
        row("Город", r.address.city),
        row("Улица", r.address.street),
        row(
          "Документы",
          r.documents.zipWithIndex
            .map((d, i) => s"${i + 1}. ${d.typ} — ${d.number}")
            .mkString("; ")
        )
      ),
      a(href := "/", `class` := "btn btn-link", text("← Вернуться к форме"))
    )
    page(content)

  val routes = Routes(
    Method.GET / "" -> handler {
      renderPage(None, Map.empty)
    },
    Method.POST / "" -> handler { (req: Request) =>
      req.body.asURLEncodedForm.map { fd =>
        decodeAndValidate[Registration](fd) match
          case Left(incomplete) =>
            renderPage(incomplete.oldForm, incomplete.errors)
          case Right(reg) => renderSuccess(reg)
      }
    }
  )

  def run: ZIO[Any, Throwable, Nothing] =
    Server.serve(routes.sandbox).provide(Server.defaultWithPort(8080))
