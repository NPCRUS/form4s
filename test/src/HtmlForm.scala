package form4s

import zio.http.template2.{form as formTag, *}
import zio.http.template2.Dom

object HtmlForm extends Form[Dom] {
  def base: Dom = div
  def amend(p: Dom)(inside: Dom*): Dom = Dom.fragment((p +: inside)*)
  def render(in: Dom): String = in.render(true)
  def subFormContainer(label: String, errors: Seq[String]): Dom =
    div(`class` := "subform", text(label), errors.map(e => span(`class` := "error", text(e))))
  def addBtn: Dom = button(`type` := "button", text("Add"))
  def deleteBtn: Dom = button(`type` := "button", text("Delete"))
  def listOfSubformsContainer(
      label: String,
      fieldName: Cursor,
      items: Seq[Dom],
      templateItem: Dom,
      errors: Seq[String]
  ): Dom = div(errors.map(e => span(`class` := "error", text(e))), items, templateItem, addBtn)

  val stringRenderable: Renderable[String] = new Renderable[String] {
    def draw(
        schema: FieldSchema[String],
        fieldName: Cursor,
        oldValue: Option[String],
        errors: Seq[String]
    ): Dom =
      div(
        label(`for` := fieldName.build, text(schema.label)),
        input(
          name := fieldName.build,
          `type` := schema.typeAttr,
          placeholder := schema.placeholderAttr,
          oldValue.map(v => value := v)
        ),
        errors.map(e => span(`class` := "error", text(e)))
      )
  }

  val intRenderable: Renderable[Int] = new Renderable[Int] {
    def draw(
        schema: FieldSchema[Int],
        fieldName: Cursor,
        oldValue: Option[Int],
        errors: Seq[String]
    ): Dom =
      div(
        label(`for` := fieldName.build, text(schema.label)),
        input(
          name := fieldName.build,
          `type` := schema.typeAttr,
          placeholder := schema.placeholderAttr,
          oldValue.map(v => value := v.toString)
        ),
        errors.map(e => span(`class` := "error", text(e)))
      )
  }

  val boolRenderable: Renderable[Boolean] = new Renderable[Boolean] {
    def draw(
        schema: FieldSchema[Boolean],
        fieldName: Cursor,
        oldValue: Option[Boolean],
        errors: Seq[String]
    ): Dom =
      div(
        label(`for` := fieldName.build, text(schema.label)),
        input(
          name := fieldName.build,
          `type` := "checkbox",
          oldValue.filter(identity).map(_ => checked)
        ),
        errors.map(e => span(`class` := "error", text(e)))
      )
  }
}
