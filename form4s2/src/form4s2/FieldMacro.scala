package form4s2

import zio.blocks.schema.{Lens, Schema}

import scala.quoted.*

object FieldMacro:

  def fieldImpl[S: Type, A: Type](sel: Expr[S => A])(using
      Quotes
  ): Expr[LeafField[S, A, ?]] =
    import quotes.reflect.*

    val name = extractName(sel.asTerm)

    val schemaExpr = Expr
      .summon[Schema[S]]
      .getOrElse(
        report.errorAndAbort(
          s"No given Schema[${Type.show[S]}] in scope. Add `given Schema[...] = Schema.derived`."
        )
      )

    val lensExpr: Expr[Lens[S, A]] =
      '{
        val rec = $schemaExpr.reflect.asRecord
          .getOrElse(
            sys.error(s"${$schemaExpr} is not a record")
          )
        rec
          .lensByName[A](${ Expr(name) })
          .getOrElse(sys.error(${ Expr(s"No field named '$name'") }))
      }

    val body: Expr[LeafField[S, A, Nothing]] =
      '{ LeafField.of[S, A]($lensExpr, ${ Expr(name) }) }

    val nameType = ConstantType(StringConstant(name))
    val tpe =
      TypeRepr
        .of[LeafField]
        .appliedTo(
          List(TypeRepr.of[S], TypeRepr.of[A], nameType)
        )

    tpe.asType match
      case '[t] =>
        val typed = Typed(body.asTerm, TypeTree.of[t])
        typed.asExpr.asInstanceOf[Expr[LeafField[S, A, ?]]]

  private def extractName(using Quotes)(term: quotes.reflect.Term): String =
    import quotes.reflect.*

    def loop(t: Term): String =
      t match
        case Inlined(_, _, inlined)                      => loop(inlined)
        case Block(Nil, e)                               => loop(e)
        case Block(List(DefDef(_, _, _, Some(body))), _) => loop(body)
        case Lambda(_, body)                             => loop(body)
        case Typed(e, _)                                 => loop(e)
        case Select(_, name)                             => name
        case other                                       =>
          report.errorAndAbort(
            s"Expected a simple field selector like _.name, got '${other.show}'"
          )

    loop(term)
