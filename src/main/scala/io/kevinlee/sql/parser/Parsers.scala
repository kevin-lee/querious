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

  val AlphabetLower: NamedFunction[Char, Boolean] =
    NamedFunction('a' to 'z' contains (_: Char), "AlphabetLower")

  val AlphabetUpper: NamedFunction[Char, Boolean] =
    NamedFunction('A' to 'Z' contains (_: Char), "AlphabetUpper")

  val Whitespace: NamedFunction[Char, Boolean] =
    NamedFunction(" \t\r\n" contains (_: Char), "Whitespace")

  val Digit: NamedFunction[Char, Boolean] =
    NamedFunction('0' to '9' contains (_: Char), "Digit")

  val alphabetLower: P[String] = P(CharsWhile(AlphabetLower).!)

  val alphabetUpper: P[String] = P(CharsWhile(AlphabetUpper).!)

  val digit: P[String] = P( CharsWhile(Digit).!)

//  var alphaNumeric: P[String] = P((CharsWhile(Digit) | CharsWhile(AlphabetLower)).rep(1).!)
  var alphaNumeric: P[String] = P(digit | alphabetLower | alphabetUpper).rep(1).!


  """
    |SELECT *
    |  FROM items
    | WHERE item_id = 1 AND (price > 100 OR name = 'awesome product')
  """.stripMargin

}
