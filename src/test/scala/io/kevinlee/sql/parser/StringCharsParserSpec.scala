package io.kevinlee.sql.parser

import hedgehog._
import hedgehog.runner._
import StringInterpolation._
import fastparse.core.Parsed.{Failure, Success}
import eu.timepit.refined.auto._
import fastparse.core.Parsed


/**
 * @author Kevin Lee
 * @since 2017-07-22
 */
object StringCharsParserSpec extends Properties {

  override def tests: List[Test] = List(
    example(
      raw"""Parsers.stringChars.parse("''") return Failure(_, 0, _)""",
      testParsersStringCharsParseTwoSingleQuotes
    ),
    example(
      raw"""Parsers.stringChars.parse("\\") return Failure(_, 0, _)""",
      testParsersStringCharsParseBackslashStr
    ),
    property(
      raw"""Parsers.stringChars.parse(alphaNumStr) return Success(alphaNumStr, alphaNumStr.length)""",
      testParsersStringCharsParseAlphaNumStr
    ),
    property(
      raw"""Parsers.stringChars.parse(one of ${TestData.escapingCharsToString}) return Failure(_, 0, _)""",
      testParsersStringCharsParseOneOfEscapingChars
    )
  )

  def testParsersStringCharsParseTwoSingleQuotes: Result = {
    val actual = Parsers.stringChars.parse("''")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testParsersStringCharsParseBackslashStr: Result = {
    val actual = Parsers.stringChars.parse("\\")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testParsersStringCharsParseAlphaNumStr: Property = for {
    value <- Gens.genNonEmptyAlphaNumString(10).log("value")
  } yield {
    val expectedValue = value.value
    val expected: Success[String, Char, String] = Success(expectedValue, expectedValue.length)
    val actual: Parsed[String, Char, String] = Parsers.stringChars.parse(value.value)
    actual ==== expected
  }

  def testParsersStringCharsParseOneOfEscapingChars: Property = for {
    value <- Gen.elementUnsafe(TestData.escapingChars).log("value")
  } yield {
    val actual = Parsers.stringChars.parse(value)
    actual matchPattern { case Failure(_, 0, _) => }
  }

}
