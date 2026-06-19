package form4s

import zio.http.template2.{form as formTag, *}

object HtmlForm extends Form[Dom] {
  def compose(out: Dom*): Dom = fragment(out*)

  val stringRenderable: Renderable[String] = new Renderable[String] {
    def draw(
        schema: FieldSchema[String],
        fieldName: String,
        oldValue: Option[String],
        errors: Seq[String]
    ): Dom =
      div(
        label(`for` := fieldName, text(schema.label)),
        input(
          name := fieldName,
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
        fieldName: String,
        oldValue: Option[Int],
        errors: Seq[String]
    ): Dom =
      div(
        label(`for` := fieldName, text(schema.label)),
        input(
          name := fieldName,
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
        fieldName: String,
        oldValue: Option[Boolean],
        errors: Seq[String]
    ): Dom =
      div(
        label(`for` := fieldName, text(schema.label)),
        input(
          name := fieldName,
          `type` := "checkbox",
          oldValue.filter(identity).map(_ => checked)
        ),
        errors.map(e => span(`class` := "error", text(e)))
      )
  }
}
