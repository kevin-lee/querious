package io.kevinlee.sql.parser

import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import hedgehog._
import hedgehog.runner._

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
object BooleanPredicateParserSpec extends Properties {

  override def tests: List[Test] = List(
    property(
      """Parsers.booleanPredicateParser handle Ne when booleanPredicateParser.parse("field = boolean value")
        |  should return Success(BooleanPredicates.Eq(field, boolean value), input.length)""".stripMargin,
      testParsersBooleanPredicateParserHandleNe
    ),
    property(
      """Parsers.booleanPredicateParser handle Ne when booleanPredicateParser.parse("field != boolean value")
        |  should return Success(BooleanPredicates.Ne(field, boolean value), input.length)""".stripMargin,
      testParsersBooleanPredicateParserHandleNe
    )
  )

  def testParsersBooleanPredicateParserHandleEq: Property =
    for {
      firstChar <- Gen
        .elementUnsafe(('_' +: (('a' to 'z') ++ ('A' to 'Z'))).toList)
        .log("firstChar")
      rest <- Gen
        .elementUnsafe(
          ('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toList)
        )
        .list(Range.linear(1, 10))
        .map(_.mkString)
        .log("rest")
      value <- Gen.boolean.log("value")

    } yield {

      val field = firstChar + rest
      val input = s"""$field = $value"""

      val expected: Parsed[BooleanPredicates.BooleanPredicate, Char, String] =
        Success(BooleanPredicates.Eq(field, value), input.length)
      val actual: Parsed[BooleanPredicates.BooleanPredicate, Char, String] =
        Parsers.booleanPredicateParser.parse(input)

      actual ==== expected
    }

  def testParsersBooleanPredicateParserHandleNe: Property =
    for {
      firstChar <- Gen
        .elementUnsafe(('_' +: (('a' to 'z') ++ ('A' to 'Z'))).toList)
        .log("firstChar")
      rest <- Gen
        .elementUnsafe(
          ('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toList)
        )
        .list(Range.linear(1, 10))
        .map(_.mkString)
        .log("rest")
      value <- Gen.boolean.log("value")

    } yield {

      val field = firstChar + rest
      val input = s"""$field != $value"""

      val expected: Parsed[BooleanPredicates.BooleanPredicate, Char, String] =
        Success(BooleanPredicates.Ne(field, value), input.length)

      val actual: Parsed[BooleanPredicates.BooleanPredicate, Char, String] =
        Parsers.booleanPredicateParser.parse(input)
      actual ==== expected
    }

}
