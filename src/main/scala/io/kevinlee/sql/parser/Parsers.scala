package io.kevinlee.sql.parser

import fastparse.all._

import scala.language.postfixOps

/**
  * @author Kevin Lee
  * @since 2017-07-15
  */
object Parsers {

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T) = f(t)
    override val toString: String = name
  }

  val `true`: P[Boolean] = P("true").map(_ => true)
  val `false`: P[Boolean] = P("false").map(_ => false)

  val AlphabetLower: NamedFunction[Char, Boolean] =
    NamedFunction('a' to 'z' contains (_: Char), "AlphabetLower")

  val AlphabetUpper: NamedFunction[Char, Boolean] =
    NamedFunction('A' to 'Z' contains (_: Char), "AlphabetUpper")

  val Whitespace: NamedFunction[Char, Boolean] =
    NamedFunction(" \t\r\n" contains (_: Char), "Whitespace")

  val Digit: NamedFunction[Char, Boolean] =
    NamedFunction('0' to '9' contains (_: Char), "Digit")

  val StringChar: NamedFunction[Char, Boolean] =
    NamedFunction(!"''\\".contains(_: Char), "StringChar")

  val alphabetsLower: P[String] = P(CharsWhile(AlphabetLower).!)

  val alphabetsUpper: P[String] = P(CharsWhile(AlphabetUpper).!)

  val alphabets: P[String] = P(alphabetsLower | alphabetsUpper).rep(1).!

  val digits: P[String] = P( CharsWhile(Digit).!)

  val alphaNumerics: P[String] = P(digits | alphabetsLower | alphabetsUpper).rep(1).!

  val spaces: P[String] = P(CharsWhile(Whitespace).!)

  /*
   * \" \/ \\ \b \f \n \r \t
   */
  val escape: P[String] =
    P("""\""" ~ CharIn("""/"\bfnrt""")).! | P("''").map(_ => "'")

  val stringChars: P[String] = P(CharsWhile(StringChar)).!
  val strings: P[String] = P("'" ~/ (stringChars | escape).rep.map(_.mkString) ~ "'")

  """
    |SELECT *
    |  FROM items
    | WHERE item_id = 1 AND (price > 100 OR name = 'awesome product')
  """.stripMargin

}
