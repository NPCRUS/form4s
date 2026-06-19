package demo

import zio.http.template2.{form as formTag, *}
import form4s.Form
import java.util.UUID

object DemoHtmlForm extends Form[Dom] {
  def compose(out: Dom*): Dom = fragment(out*)

  private val inputCls =
    "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm border p-2"
  private val inputErrCls =
    "mt-1 block w-full rounded-md border-red-500 shadow-sm focus:border-red-500 focus:ring-red-500 sm:text-sm border p-2"
  private val labelCls = "block text-sm font-medium text-gray-700"
  private val errorCls = "mt-1 text-sm text-red-600"

  val stringRenderable: Renderable[String] = new Renderable[String] {
    def draw(
        schema: FieldSchema[String],
        fieldName: String,
        oldValue: Option[String],
        errors: Seq[String]
    ): Dom =
      div(
        `class` := "mb-4",
        label(`for` := fieldName, `class` := labelCls, text(schema.label)),
        input(
          name := fieldName,
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
        fieldName: String,
        oldValue: Option[Int],
        errors: Seq[String]
    ): Dom =
      div(
        `class` := "mb-4",
        label(`for` := fieldName, `class` := labelCls, text(schema.label)),
        input(
          name := fieldName,
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
        fieldName: String,
        oldValue: Option[Long],
        errors: Seq[String]
    ): Dom =
      div(
        `class` := "mb-4",
        label(`for` := fieldName, `class` := labelCls, text(schema.label)),
        input(
          name := fieldName,
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
        fieldName: String,
        oldValue: Option[UUID],
        errors: Seq[String]
    ): Dom =
      div(
        `class` := "mb-4",
        label(`for` := fieldName, `class` := labelCls, text(schema.label)),
        input(
          name := fieldName,
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
        fieldName: String,
        oldValue: Option[Boolean],
        errors: Seq[String]
    ): Dom =
      div(
        `class` := "mb-4 flex items-center gap-2",
        input(
          name := fieldName,
          `type` := "checkbox",
          `class` := "h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-500",
          oldValue.filter(identity).map(_ => checked)
        ),
        label(
          `for` := fieldName,
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
          fieldName: String,
          oldValue: Option[T],
          errors: Seq[String]
      ): Dom = {
        val oldStr = oldValue.map(show)
        div(
          `class` := "mb-4",
          label(`for` := fieldName, `class` := labelCls, text(schema.label)),
          select(
            name := fieldName,
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
