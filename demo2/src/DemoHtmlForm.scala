package demo

import zio.http.template2.{form as formTag, *}
import form4s.FormV2
import java.util.UUID
import zio.http.template2.Dom.Element
import zio.http.template2.Dom.Fragment
import form4s.Cursor

object DemoHtmlForm extends FormV2[Element] {
  def base: Element = div
  def amend(in: Element)(inside: Element*): Element = in(inside)

  def render(in: Element): String = in.render(true)

  def subFormContainer(schemaLabel: String): Element =
    div(
      `class` := "mb-4 border p-4 rounded-lg bg-gray-50",
      label(
        `class` := "block text-md font-semibold text-gray-700 mb-2",
        schemaLabel
      )
    )

  def addBtn: Element =
    button(
      `type` := "button",
      `class` := "mt-2 inline-flex items-center px-3 py-1 border border-transparent text-sm font-medium rounded-md text-indigo-700 bg-indigo-100 hover:bg-indigo-200",
      custom("onclick") := "form4s.addItem(this)",
      text("+ Добавить")
    )

  def deleteBtn: Element =
    button(
      `type` := "button",
      `class` := "text-red-600 hover:text-red-900 ml-2",
      custom("onclick") := "form4s.deleteItem(this)",
      text("✕")
    )

  def listOfSubformsContainer(
      fieldLabel: String,
      fieldName: Cursor,
      items: Seq[Element],
      templateItem: Element
  ): Element =
    div(
      data("repeated") := fieldName.build,
      `class` := "mb-4 border p-4 rounded-lg bg-gray-50",
      label(
        `class` := "block text-md font-semibold text-gray-700 mb-2",
        fieldLabel
      ),
      items.map(i => div(data("repeated-item") := "", i)),
      template(div(data("repeated-item") := "", templateItem)),
      addBtn,
      script.inlineJs(
        "(function(){if(window.form4s)return;window.form4s={addItem:function(b){var c=b.closest('[data-repeated]');var t=c.querySelector('template');t.before(t.content.cloneNode(true))},deleteItem:function(b){var i=b.closest('[data-repeated-item]');if(i)i.remove()}}})()"
      )
    )

  val inputCls: String =
    "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm border p-2"
  val inputErrCls: String =
    "mt-1 block w-full rounded-md border-red-500 shadow-sm focus:border-red-500 focus:ring-red-500 sm:text-sm border p-2"
  val labelCls: String = "block text-sm font-medium text-gray-700"
  val errorCls: String = "mt-1 text-sm text-red-600"

  val stringRenderable: Renderable[String] = new Renderable[String] {
    def draw(
        schema: FieldSchema[String],
        fieldName: Cursor,
        oldValue: Option[String],
        errors: Seq[String]
    ): Element =
      div(
        `class` := "mb-4",
        label(
          `for` := fieldName.build,
          `class` := labelCls,
          text(schema.label)
        ),
        input(
          name := fieldName.build,
          `type` := schema.typeAttr,
          placeholder := schema.placeholderAttr,
          `class` := (if (errors.nonEmpty) inputErrCls else inputCls),
          oldValue.map(v => value := v)
        ),
        errors.map(e => p(`class` := errorCls, text(e)))
      )
  }

  val intRenderable: Renderable[Int] = new Renderable[Int] {
    def draw(
        schema: FieldSchema[Int],
        fieldName: Cursor,
        oldValue: Option[Int],
        errors: Seq[String]
    ): Element =
      div(
        `class` := "mb-4",
        label(
          `for` := fieldName.build,
          `class` := labelCls,
          text(schema.label)
        ),
        input(
          name := fieldName.build,
          `type` := schema.typeAttr,
          placeholder := schema.placeholderAttr,
          `class` := (if (errors.nonEmpty) inputErrCls else inputCls),
          oldValue.map(v => value := v.toString)
        ),
        errors.map(e => p(`class` := errorCls, text(e)))
      )
  }

  val longRenderable: Renderable[Long] = new Renderable[Long] {
    def draw(
        schema: FieldSchema[Long],
        fieldName: Cursor,
        oldValue: Option[Long],
        errors: Seq[String]
    ): Element =
      div(
        `class` := "mb-4",
        label(
          `for` := fieldName.build,
          `class` := labelCls,
          text(schema.label)
        ),
        input(
          name := fieldName.build,
          `type` := schema.typeAttr,
          placeholder := schema.placeholderAttr,
          `class` := (if (errors.nonEmpty) inputErrCls else inputCls),
          oldValue.map(v => value := v.toString)
        ),
        errors.map(e => p(`class` := errorCls, text(e)))
      )
  }

  val uuidRenderable: Renderable[UUID] = new Renderable[UUID] {
    def draw(
        schema: FieldSchema[UUID],
        fieldName: Cursor,
        oldValue: Option[UUID],
        errors: Seq[String]
    ): Element =
      div(
        `class` := "mb-4",
        label(
          `for` := fieldName.build,
          `class` := labelCls,
          text(schema.label)
        ),
        input(
          name := fieldName.build,
          `type` := schema.typeAttr,
          placeholder := schema.placeholderAttr,
          `class` := (if (errors.nonEmpty) inputErrCls else inputCls),
          oldValue.map(v => value := v.toString)
        ),
        errors.map(e => p(`class` := errorCls, text(e)))
      )
  }

  val boolRenderable: Renderable[Boolean] = new Renderable[Boolean] {
    def draw(
        schema: FieldSchema[Boolean],
        fieldName: Cursor,
        oldValue: Option[Boolean],
        errors: Seq[String]
    ): Element =
      div(
        `class` := "mb-4 flex items-center gap-2",
        input(
          name := fieldName.build,
          `type` := "checkbox",
          `class` := "h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-500",
          oldValue.filter(identity).map(_ => checked)
        ),
        label(
          `for` := fieldName.build,
          `class` := "text-sm text-gray-700",
          text(schema.label)
        ),
        errors.map(e => p(`class` := errorCls, text(e)))
      )
  }

  def selectRenderable[T](show: T => String): Renderable[T] =
    new Renderable[T] {
      def draw(
          schema: FieldSchema[T],
          fieldName: Cursor,
          oldValue: Option[T],
          errors: Seq[String]
      ): Element = {
        val oldStr = oldValue.map(show)
        div(
          `class` := "mb-4",
          label(
            `for` := fieldName.build,
            `class` := labelCls,
            text(schema.label)
          ),
          select(
            name := fieldName.build,
            `class` := (if (errors.nonEmpty) inputErrCls else inputCls),
            option(value := "", text("-- Выберите --")),
            schema.options.map { opt =>
              option(
                value := opt,
                oldStr.filter(_ == opt).map(_ => selected),
                text(opt)
              )
            }
          ),
          errors.map(e => p(`class` := errorCls, text(e)))
        )
      }
    }
}
