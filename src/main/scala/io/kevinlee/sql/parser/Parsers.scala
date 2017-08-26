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

  val digit: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

  val AlphabetLower = NamedFunction('a' to 'z' contains (_: Char), "AlphabetLower")

  val AlphabetUpper = NamedFunction('A' to 'Z' contains (_: Char), "AlphabetUpper")

  val Whitespace = NamedFunction(" \t\r\n" contains (_: Char), "Whitespace")

  val Digit = NamedFunction('0' to '9' contains (_: Char), "Digit")

  """
    |SELECT *
    |  FROM items
    | WHERE item_id = 1 AND (price > 100 OR name = 'awesome product')
  """.stripMargin

}
