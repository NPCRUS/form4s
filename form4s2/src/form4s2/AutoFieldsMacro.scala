package form4s2

import zio.blocks.schema.Schema

import scala.quoted.*

object AutoFieldsMacro:

  def fieldsForImpl[S: Type](schema: Expr[Schema[S]])(using
      Quotes
  ): Expr[FieldsFor[S]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[S].dealias
    val sym = tpe.typeSymbol
    val fields = sym.caseFields

    var refined: TypeRepr = TypeRepr.of[FieldsHolder]
    val names = List.newBuilder[String]
    for field <- fields do
      val name = field.name
      names += name
      val fieldType = tpe.memberType(field).dealias
      val leafType = TypeRepr
        .of[LeafField]
        .appliedTo(
          List(tpe, fieldType, ConstantType(StringConstant(name)))
        )
      refined = Refinement(refined, name, leafType)

    val namesExpr = Expr(names.result())

    refined.asType match
      case '[rt] =>
        val holder =
          '{
            val record = $schema.reflect.asRecord
              .getOrElse(
                sys.error(s"${$schema} is not a record")
              )
            val members = $namesExpr.map { n =>
              n -> LeafField
                .of[S, Any](
                  record
                    .lensByName[Any](n)
                    .getOrElse(sys.error(s"No field '$n'")),
                  n
                )
            }.toMap
            new FieldsHolder(members)
          }
        val typedHolder =
          Typed(holder.asTerm, TypeTree.of[rt]).asExpr.asInstanceOf[Expr[rt]]
        '{
          new FieldsFor[S] {
            type Out = rt
            def fields = $typedHolder
          }
        }
