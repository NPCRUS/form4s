package example

import form4s2.*
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

object Address extends FormFields[Address]:
  given Schema[Address] = Schema.derived
  val city = field(_.city)
  val street = field(_.street)

case class Document(typ: String, number: String)

object Document extends FormFields[Document]:
  given Schema[Document] = Schema.derived
  val typ = field(_.typ)
  val number = field(_.number)

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

object Registration extends FormFields[Registration]:
  given Schema[Registration] = Schema.derived
  val username = field(_.username)
  val email = field(_.email)
  val age = field(_.age)
  val score = field(_.score)
  val referralId = field(_.referralId)
  val role = field(_.role)
  val bio = field(_.bio)
  val agree = field(_.agree)
  val address = field(_.address)
  val documents = field(_.documents)
