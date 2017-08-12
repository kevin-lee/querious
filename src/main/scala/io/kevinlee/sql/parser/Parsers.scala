package io.kevinlee.sql.parser

import fastparse.all._

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

  val alphabetLower = NamedFunction('a' to 'z' contains (_: Char), "AlphabetLower" )

}
