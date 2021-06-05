package io.kevinlee.sql.parser

import fastparse.all._
import io.kevinlee.sql.parser.BooleanPredicates.BooleanPredicate
import io.kevinlee.sql.parser.NumberPredicates.NumberPredicate
import io.kevinlee.sql.parser.StringPredicates.StringPredicate

/**
  * @author Kevin Lee
  * @since 2017-07-15
  */
object Parsers {

  final val WhitespaceChars: List[Char] = List(
    '\t',
    '\n',
    '\u000b',
    '\f',
    '\r',
    '\u001c',
    '\u001d',
    '\u001e',
    '\u001f',
    ' ',
    '\u1680',
    '\u2000',
    '\u2001',
    '\u2002',
    '\u2003',
    '\u2004',
    '\u2005',
    '\u2006',
    '\u2008',
    '\u2009',
    '\u200a',
    '\u2028',
    '\u2029',
    '\u205f',
    '\u3000'
  )

  final val NonWhitespaceCharRange: List[(Int, Int)] =
    List(0 -> 8, 14 -> 27, 33 -> 5759, 5761 -> 8191, 8199 -> 8199, 8203 -> 8231, 8234 -> 8286, 8288 -> 12287, 12289 -> Char.MaxValue.toInt)

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T): V = f(t)
    override val toString: String = name
  }

  val `true`: P[Boolean] = P("true").map(_ => true)
  val `false`: P[Boolean] = P("false").map(_ => false)

  val AlphabetLower: NamedFunction[Char, Boolean] =
    NamedFunction('a' to 'z' contains (_: Char), "AlphabetLower")

  val AlphabetUpper: NamedFunction[Char, Boolean] =
    NamedFunction('A' to 'Z' contains (_: Char), "AlphabetUpper")

  val Whitespace: NamedFunction[Char, Boolean] =
    NamedFunction(WhitespaceChars.contains(_: Char), "Whitespace")

  val Digit: NamedFunction[Char, Boolean] =
    NamedFunction('0' to '9' contains (_: Char), "Digit")

  val StringChar: NamedFunction[Char, Boolean] =
    NamedFunction(!"''\\".contains(_: Char), "StringChar")

  val alphabetsLower: P[String] = P(CharsWhile(AlphabetLower).!)

  val alphabetsUpper: P[String] = P(CharsWhile(AlphabetUpper).!)

  val alphabets: P[String] = P(alphabetsLower | alphabetsUpper).rep(1).!

  val digits: P[Unit] = P(CharsWhile(Digit))

  val expondent: P[Unit] = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val factional: P[Unit] = P(CharIn(".") ~ digits)
  val integral: P[Unit] = P(("0" | CharIn('1' to '9')) ~ digits.?)

  val numbers: P[BigDecimal] =
    P(CharIn("+-").? ~ integral ~ factional.? ~ expondent.?).!.map(BigDecimal(_))

  val alphaNumerics: P[String] = P(digits | alphabetsLower | alphabetsUpper).rep(1).!

  val spaces: P[Unit] = P(CharsWhile(Whitespace))

  val hexDigit: P[Unit] = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val unicodeEscape: P[Unit] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  /*
   * \" \/ \\ \b \f \n \r \t
   * OR
   * Escaped unicode chars like \u0000
   * OR
   * ''
   */
  val escape: P[Any] =
    P("""\""" ~ (CharIn("""/"\bfnrt""") | unicodeEscape)) | P("''").map(_ => "'")

  val stringChars: P[String] = P(CharsWhile(StringChar)).!
  val strings: P[String] = P("'" ~/ (stringChars | escape).rep.map(_.mkString) ~ "'")

  /*
   * a_test
   * ab_test
   * A_TEST
   * AB_TEST
   * _ABC
   * _abc
   * a_test1
   * a_1test1
   */
  val identifier: P[String] = P((alphabets | "_") ~ P(alphaNumerics | "_").rep).!

  val equalitySigns: P[Unit] = "=" | "!="
  val comparisonSigns: P[Unit] = equalitySigns | "<=" | "<" | ">=" | ">"

  /*
   * a = 10
   * a != 10
   * a_test = 'abc'
   * b = false
   */
  val booleanPredicateParser: P[BooleanPredicate] = P(identifier.! ~ spaces.? ~ equalitySigns.! ~ spaces.? ~ (`true` | `false`)) map {
    case (field, "=", booleanValue) => BooleanPredicates.Eq(field, booleanValue)
    case (field, "!=", booleanValue) => BooleanPredicates.Ne(field, booleanValue)
  }

  /*
   * a = 10
   * a != 10
   * a < 10
   * a <= 10
   * a > 10
   * a >= 10
   */
  val numberPredicateParser: P[NumberPredicate] = P(identifier.! ~ spaces.? ~ comparisonSigns.! ~ spaces.? ~ numbers) map {
    case (field, "=", numberValue)  => NumberPredicates.Eq(field, numberValue)
    case (field, "!=", numberValue) => NumberPredicates.Ne(field, numberValue)
    case (field, "<", numberValue)  => NumberPredicates.Lt(field, numberValue)
    case (field, "<=", numberValue) => NumberPredicates.Le(field, numberValue)
    case (field, ">", numberValue)  => NumberPredicates.Gt(field, numberValue)
    case (field, ">=", numberValue) => NumberPredicates.Ge(field, numberValue)
  }

  val stringPredicateParser: P[StringPredicate] = P(identifier.! ~ spaces.? ~ comparisonSigns.! ~ spaces.? ~ strings) map {
    case (field, "=", stringValue)  => StringPredicates.Eq(field, stringValue)
    case (field, "!=", stringValue) => StringPredicates.Ne(field, stringValue)
    case (field, "<", stringValue)  => StringPredicates.Lt(field, stringValue)
    case (field, "<=", stringValue) => StringPredicates.Le(field, stringValue)
    case (field, ">", stringValue)  => StringPredicates.Gt(field, stringValue)
    case (field, ">=", stringValue) => StringPredicates.Ge(field, stringValue)
  }

  val predicates: P[Clause] = P(booleanPredicateParser | numberPredicateParser | stringPredicateParser)

  val parens: P[Clause] = P("(" ~/ spaces.? ~ clause ~ spaces.? ~ ")")

  val predicateOrParens = P(predicates | parens)

  val clause: P[Clause] = P(predicateOrParens ~ (spaces.rep(1) ~ (StringInIgnoreCase("and") | StringInIgnoreCase("or")).! ~ spaces.rep(1) ~ predicateOrParens).rep.?) map eval

  val where: P[Clause] = StringInIgnoreCase("where") ~ spaces.rep(1) ~ clause

  def eval(clauses: (Clause, Option[Seq[(String, Clause)]])): Clause = clauses match {
      case (first, maybeRest) =>
        def eval(first: Clause,
                 maybeRest: Option[Seq[(String, Clause)]]): Clause = maybeRest match {
            case Some((junction, clause) +: tail) if junction.equalsIgnoreCase("and") =>
              eval(And(first, clause), Some(tail))
            case Some((junction, clause) +: tail) if junction.equalsIgnoreCase("or") =>
              Or(first, eval(clause, Some(tail)))
            case Some(Seq()) =>
              first
            case None =>
              first
          }
        eval(first, maybeRest)
    }

}
