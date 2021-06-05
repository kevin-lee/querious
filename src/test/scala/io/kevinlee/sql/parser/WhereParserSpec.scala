package io.kevinlee.sql.parser

import TestData._
import fastparse.all.Parsed
import fastparse.core.Parsed.Success
import hedgehog._
import hedgehog.runner._

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
object WhereParserSpec extends Properties {

  override def tests: List[Test] = List(
    property("test Parsers.where.parse", testWhereParser)
  )

  def testWhereParser: Property =
    for {
      (whereClause, expectedClause) <- Gen
        .elementUnsafe(whereClauseData)
        .log("(whereClause, expectedClause)")
    } yield {
      val input = s"WHERE $whereClause"
      val expected: Parsed[Clause] = Success(expectedClause, input.length)
      val actual = Parsers.where.parse(input)
      actual ==== expected

    }

}
