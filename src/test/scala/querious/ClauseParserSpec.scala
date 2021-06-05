package querious

import TestData._
import fastparse.all.Parsed
import fastparse.core.Parsed.Success
import hedgehog._
import hedgehog.runner._

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
object ClauseParserSpec extends Properties {

  override def tests: List[Test] = List(
    property("Parsers.clause.parse", testClauseParser)
  )

  def testClauseParser: Property =
    for {
      (input, expectedClause) <- Gen
        .elementUnsafe(whereClauseData)
        .log("(input, expectedClause)")
    } yield {
      val expected: Parsed[Clause] = Success(expectedClause, input.length)
      val actual = Parsers.clause.parse(input)
      actual ==== expected

    }

}
