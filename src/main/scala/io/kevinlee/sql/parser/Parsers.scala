package io.kevinlee.sql.parser

import fastparse.all._

/**
  * @author Kevin Lee
  * @since 2017-07-15
  */
object Parsers {
  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
}
