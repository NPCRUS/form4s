package demo

import form4s.*
import zio.http.template2.*

object Views:

  private val okCls = "form-control"
  private val errCls = "form-control is-invalid"

  private def inputClass(ctx: RenderContext[?]): String =
    if ctx.error.nonEmpty then errCls else okCls

  private def errsOf(ctx: RenderContext[?]): Seq[Dom] =
    ctx.error.map(e => div(`class` := "invalid-feedback", text(e)))

  private def fieldWrap(children: Seq[Dom]): List[Dom] =
    List(div(`class` := "mb-3", children))

  def textInput(
      ctx: RenderContext[String],
      labelText: String,
      placeholderText: String = "",
      isRequired: Boolean = false
  ): List[Dom] =
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        input(
          name := ctx.path,
          `type` := "text",
          placeholder := placeholderText,
          `class` := inputClass(ctx),
          value := ctx.oldValue.getOrElse(""),
          Option.when(isRequired)(required)
        )
      ) ++ errsOf(ctx)
    )

  def emailInput(
      ctx: RenderContext[String],
      labelText: String,
      placeholderText: String = "",
      isRequired: Boolean = false
  ): List[Dom] =
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        input(
          name := ctx.path,
          `type` := "email",
          placeholder := placeholderText,
          `class` := inputClass(ctx),
          value := ctx.oldValue.getOrElse(""),
          Option.when(isRequired)(required)
        )
      ) ++ errsOf(ctx)
    )

  def numberInput(ctx: RenderContext[Long], labelText: String): List[Dom] =
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        input(
          name := ctx.path,
          `type` := "number",
          `class` := inputClass(ctx),
          value := ctx.oldValue.map(_.toString).getOrElse("")
        )
      ) ++ errsOf(ctx)
    )

  def intOptionInput(
      ctx: RenderContext[Option[Int]],
      labelText: String
  ): List[Dom] =
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        input(
          name := ctx.path,
          `type` := "number",
          `class` := inputClass(ctx),
          value := ctx.oldValue.flatten.map(_.toString).getOrElse("")
        )
      ) ++ errsOf(ctx)
    )

  def uuidInput(
      ctx: RenderContext[java.util.UUID],
      labelText: String
  ): List[Dom] =
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        input(
          name := ctx.path,
          `type` := "text",
          placeholder := "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
          `class` := inputClass(ctx),
          value := ctx.oldValue.map(_.toString).getOrElse("")
        )
      ) ++ errsOf(ctx)
    )

  def textArea(
      ctx: RenderContext[Option[String]],
      labelText: String
  ): List[Dom] =
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        textarea(
          name := ctx.path,
          rows := "3",
          `class` := inputClass(ctx),
          text(ctx.oldValue.flatten.getOrElse(""))
        )
      ) ++ errsOf(ctx)
    )

  def checkbox(ctx: RenderContext[Boolean], labelText: String): List[Dom] =
    List(
      div(
        `class` := "mb-3",
        div(
          `class` := "form-check",
          input(
            name := ctx.path,
            `type` := "checkbox",
            `class` := (if ctx.error.nonEmpty then "form-check-input is-invalid"
                        else "form-check-input"),
            ctx.oldValue.filter(identity).map(_ => checked)
          ),
          label(
            `for` := ctx.path,
            `class` := "form-check-label",
            text(labelText)
          ),
          errsOf(ctx)
        )
      )
    )

  def roleSelect(ctx: RenderContext[Role], labelText: String): List[Dom] =
    val opts = Role.values.toList.map(r =>
      option(
        value := r.toString,
        ctx.oldValue.filter(_ == r).map(_ => selected),
        text(Role.label(r))
      )
    )
    fieldWrap(
      Seq(
        label(`for` := ctx.path, `class` := "form-label", text(labelText)),
        select(
          name := ctx.path,
          `class` := (if ctx.error.nonEmpty then "form-select is-invalid"
                      else "form-select"),
          option(value := "", text("-- Выберите --")),
          opts
        )
      ) ++ errsOf(ctx)
    )

  def section(title: String)(children: List[Dom]): List[Dom] =
    List(
      div(
        `class` := "card mb-3",
        div(`class` := "card-header", text(title)),
        div(`class` := "card-body", children)
      )
    )

  def repeatedSection(
      title: String,
      basePath: String,
      items: List[Dom],
      tpl: List[Dom],
      nextIndex: Int
  ): List[Dom] =
    List(
      div(
        `class` := "card mb-3",
        data("repeated") := basePath,
        data("next-index") := nextIndex.toString,
        div(`class` := "card-header", text(title)),
        div(
          `class` := "card-body",
          items,
          template(tpl),
          button(
            `type` := "button",
            `class` := "btn btn-outline-primary btn-sm mt-2",
            custom("onclick") := "form4s2demo.addItem(this)",
            text("+ Добавить документ")
          )
        ),
        script.inlineJs(
          "(function(){if(window.form4s2demo)return;window.form4s2demo={addItem:function(b){var c=b.closest('[data-repeated]');var t=c.querySelector('template');var n=parseInt(c.getAttribute('data-next-index'));c.setAttribute('data-next-index',n+1);var f=t.content.cloneNode(true);var re=/\\.0\\./g;var rp='.'+n+'.';f.querySelectorAll('[name]').forEach(function(e){e.name=e.name.replace(re,rp)});f.querySelectorAll('[for]').forEach(function(e){e.setAttribute('for',e.getAttribute('for').replace(re,rp))});f.querySelectorAll('[id]').forEach(function(e){e.id=e.id.replace(re,rp)});t.before(f)},deleteItem:function(b){var i=b.closest('[data-item]');if(i)i.remove()}}})()"
        )
      )
    )

  val addressForm: FormRenderer[Address, List[Dom]] =
    draw(
      drawField(Address.city)(ctx => textInput(ctx, "Город", "Введите город")),
      drawField(Address.street)(ctx => textInput(ctx, "Улица", "Введите улицу"))
    )(_ ++ _)

  val documentForm: FormRenderer[Document, List[Dom]] =
    draw(
      drawField(Document.typ)(ctx =>
        textInput(ctx, "Тип документа", "Например, паспорт")
      ),
      drawField(Document.number)(ctx =>
        textInput(ctx, "Номер документа", "Введите номер")
      )
    )(_ ++ _)

  def docsSection(ctx: RenderContext[Seq[Document]]): List[Dom] =
    val basePath = ctx.path
    val items = ctx.oldValue.getOrElse(Seq.empty)
    val rendered = items.zipWithIndex.map { (doc, i) =>
      div(
        data("item") := "",
        `class` := "mb-3 p-2 border rounded",
        documentForm(RenderContext(ctx.at(i), Some(doc), ctx.errors)),
        button(
          `type` := "button",
          `class` := "btn btn-outline-danger btn-sm",
          custom("onclick") := "form4s2demo.deleteItem(this)",
          text("Удалить")
        )
      )
    }.toList
    val tpl = documentForm(RenderContext(ctx.at(0), None, Map.empty))
    val sectionErrs =
      ctx.error
        .map(e => div(`class` := "text-danger small mb-2", text(e)))
        .toList
    repeatedSection(
      "Документы",
      basePath,
      sectionErrs ++ rendered,
      tpl,
      items.size
    )

  val registrationForm: FormRenderer[Registration, List[Dom]] =
    draw(
      drawField(Registration.username)(ctx =>
        textInput(ctx, "Имя пользователя", "Введите имя", isRequired = true)
      ),
      drawField(Registration.email)(ctx =>
        emailInput(ctx, "Email", "example@mail.com", isRequired = true)
      ),
      drawField(Registration.age)(ctx => intOptionInput(ctx, "Возраст")),
      drawField(Registration.score)(ctx => numberInput(ctx, "Баллы")),
      drawField(Registration.referralId)(ctx =>
        uuidInput(ctx, "Реферальный ID")
      ),
      drawField(Registration.role)(ctx => roleSelect(ctx, "Роль")),
      drawField(Registration.bio)(ctx => textArea(ctx, "О себе")),
      drawField(Registration.agree)(ctx =>
        checkbox(ctx, "Я согласен с условиями")
      ),
      drawField(Registration.address)(ctx =>
        section("Адрес")(addressForm(ctx))
      ),
      drawField(Registration.documents)(ctx => docsSection(ctx))
    )(_ ++ _)
