package form4s

import scala.quoted.*

/** Extracts the compile-time literal field name from a `Drawn[S, N, E]`. */
type NameOf[X] = X match
  case Drawn[s, n, ?] => n

/** Extracts the source type from a `Drawn[S, N, E]`. */
type SrcOf[X] = X match
  case Drawn[s, ?, ?] => s

/** Extracts the output element type from a `Drawn[S, N, E]`. */
type ElemOf[X] = X match
  case Drawn[s, ?, e] => e

/** All field names drawn by a tuple of `Drawn` values. */
type Names[T <: Tuple] = Tuple.Map[T, NameOf]

/** The source type shared by a tuple of `Drawn` values (taken from the head).
  */
type Src[T <: Tuple] = SrcOf[Tuple.Head[T]]

/** The output element type of a tuple of `Drawn` values (taken from the head).
  */
type Out[T <: Tuple] = ElemOf[Tuple.Head[T]]

/** Compile-time check that `Drawn` field names cover `Labels` exactly (no
  * missing, duplicated, or unknown fields). Fails compilation with a precise
  * message otherwise.
  */
inline def checkTotality[Labels <: Tuple, DrawnNames <: Tuple]: Unit =
  ${ form4s.TotalityMacros.checkImpl[Labels, DrawnNames] }

private object TotalityMacros:

  def checkImpl[Labels <: Tuple: Type, DrawnNames <: Tuple: Type](using
      Quotes
  ): Expr[Unit] =
    import quotes.reflect.*

    val labels = constStrings(TypeRepr.of[Labels])
    val drawn = constStrings(TypeRepr.of[DrawnNames])

    val labelSet = labels.toSet

    val missing = labelSet.diff(drawn.toSet).toList.sorted.map(n => s"'$n'")

    val duplicated = drawn
      .groupBy(identity)
      .collect {
        case (n, xs) if xs.size > 1 && labelSet.contains(n) => n
      }
      .toList
      .sorted
      .map(n => s"'$n'")

    val unknown =
      drawn.filterNot(labelSet.contains).distinct.sorted.map(n => s"'$n'")

    if missing.nonEmpty || duplicated.nonEmpty || unknown.nonEmpty then
      val parts = List(
        if missing.nonEmpty then s"missing: ${missing.mkString(", ")}" else "",
        if duplicated.nonEmpty then s"duplicated: ${duplicated.mkString(", ")}"
        else "",
        if unknown.nonEmpty then s"unknown: ${unknown.mkString(", ")}" else ""
      ).filter(_.nonEmpty)
      report.errorAndAbort(
        "Incomplete form — " + parts.mkString("; ")
      )

    '{ () }

  private def constStrings(using
      Quotes
  )(
      t: quotes.reflect.TypeRepr
  ): List[String] =
    import quotes.reflect.*

    def loop(ty: TypeRepr): List[String] =
      val d = ty.dealias
      if d =:= TypeRepr.of[EmptyTuple] then Nil
      else
        d match
          case AppliedType(_, List(head, tail)) =>
            head match
              case ConstantType(StringConstant(s)) => s :: loop(tail)
              case other                           =>
                report.errorAndAbort(
                  s"Expected a literal string type, got '${other.show}'"
                )
          case other =>
            report.errorAndAbort(
              s"Expected a tuple of literal strings, got '${other.show}'"
            )

    loop(t)
