package demo

import form4s.*
import zio.blocks.schema.Schema
import zio.http.{Form, FormField}

import java.util.UUID

enum Role:
  case Student, Developer, Designer, Manager, Other

object Role:
  def label(role: Role): String = role match
    case Role.Student   => "Студент"
    case Role.Developer => "Разработчик"
    case Role.Designer  => "Дизайнер"
    case Role.Manager   => "Менеджер"
    case Role.Other     => "Другое"

  given FormDecoder[Role] = new FormDecoder[Role]:
    def decode(form: Form): Either[Seq[DecodingError], Role] =
      form.formData.headOption.flatMap(_.stringValue) match
        case Some("") | None =>
          Left(Seq(DecodingError("", "Обязательное поле")))
        case Some(s) =>
          Role.values
            .find(_.toString == s)
            .toRight(Seq(DecodingError("", "Неизвестная роль")))

case class Address(city: String, street: String)

object Address extends AutoFields[Address]:
  given Schema[Address] = Schema.derived

case class Document(typ: String, number: String)

object Document extends AutoFields[Document]:
  given Schema[Document] = Schema.derived

case class Registration(
    username: String,
    email: String,
    age: Option[Int],
    score: Long,
    referralId: UUID,
    role: Role,
    bio: Option[String],
    agree: Boolean,
    address: Address,
    documents: Seq[Document]
)

object Registration extends AutoFields[Registration]:
  given Schema[Registration] = Schema.derived
