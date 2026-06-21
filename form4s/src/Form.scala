package form4s

trait Form[Out] {
  def compose(out: Out*): Out

  trait Renderable[T] { that =>
    def draw(
        schema: FieldSchema[T],
        fieldName: String,
        oldValue: Option[T],
        errors: Seq[String]
    ): Out

    def optional: Renderable[Option[T]] = new Renderable[Option[T]] {
      def draw(
          schema: FieldSchema[Option[T]],
          fieldName: String,
          oldValue: Option[Option[T]],
          errors: Seq[String]
      ): Out =
        that.draw(
          schema.asInstanceOf[FieldSchema[T]],
          fieldName,
          oldValue.flatten,
          errors
        )
    }
  }

  case class FieldSchema[T](
      label: String,
      renderer: Renderable[T],
      placeholderAttr: String,
      typeAttr: String,
      validator: Validator[T] = Validator.empty[T],
      options: Seq[String] = Seq.empty
  )

  def draw[T[F[_]] <: Product](
      oldValue: Option[T[[T] =>> T]],
      errors: Map[String, Seq[String]]
  )(using
      schema: T[FieldSchema]
  ) = {
    val oldValues = oldValue.map(_.productIterator.toSeq)
    val names = schema.productElementNames.toSeq
    compose(
      schema.productIterator.zipWithIndex.map { (schemaAny, idx) =>
        type Gen
        val schema = schemaAny.asInstanceOf[FieldSchema[Gen]]
        schema.renderer.draw(
          schema,
          names(idx),
          oldValues.map(v => v(idx).asInstanceOf[Gen]),
          errors.get(names(idx)).getOrElse(Seq.empty)
        )
      }.toSeq*
    )
  }

  def validate[T[F[_]] <: Product](
      formData: T[[T] =>> T]
  )(using schema: T[FieldSchema]): Map[String, Seq[String]] = {
    val schemas = schema.productIterator.toSeq
    val names = formData.productElementNames.toSeq
    formData.productIterator.zipWithIndex.map { (value, idx) =>
      type Gen
      val schema = schemas(idx).asInstanceOf[FieldSchema[Gen]]
      val errors = schema.validator.validate(value.asInstanceOf[Gen])
      (names(idx), errors)
    }.toMap
  }

  def decodeAndValidate[T[F[_]] <: Product](
      input: zio.http.Form
  )(using
      schema: T[FieldSchema],
      decoder: FormDecoder[T[[T] =>> T]]
  ): Either[Map[String, Seq[String]], T[[T] =>> T]] =
    decoder.decode(input) match {
      case Left(errors) =>
        Left(errors.groupMap(_.field)(_.message))
      case Right(decoded) =>
        val validationErrors = validate(decoded).filter(_._2.nonEmpty)
        if (validationErrors.nonEmpty) Left(validationErrors)
        else Right(decoded)
    }
}
