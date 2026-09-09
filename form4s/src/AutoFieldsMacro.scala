package form4s

import zio.blocks.schema.Schema

import java.util.concurrent.ConcurrentHashMap

import scala.quoted.*

object AutoFieldsMacro:

  private val cache = new ConcurrentHashMap[String, FieldsHolder]()

  /** Public so generated code can reference it from any call site. */
  def getOrCreate(key: String)(create: => FieldsHolder): FieldsHolder =
    val existing = cache.get(key)
    if existing != null then existing
    else
      val created = create
      val prev = cache.putIfAbsent(key, created)
      if prev != null then prev else created

  def fieldsForImpl[S: Type](schema: Expr[Schema[S]])(using
      Quotes
  ): Expr[FieldsFor[S]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[S].dealias
    val sym = tpe.typeSymbol

    if !sym.flags.is(Flags.Case) then
      report.errorAndAbort(
        s"AutoFields requires a case class, but '${tpe.show}' is not one. Use FormFields with explicit `field(_.x)` for other shapes."
      )

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
    val cacheKey = Expr(tpe.show)

    refined.asType match
      case '[rt] =>
        val holder =
          '{
            getOrCreate($cacheKey) {
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
          }
        val typedHolder =
          Typed(holder.asTerm, TypeTree.of[rt]).asExpr.asInstanceOf[Expr[rt]]
        '{
          new FieldsFor[S] {
            type Out = rt
            def fields = $typedHolder
          }
        }
