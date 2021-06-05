package querious

import StringInterpolation._
import fastparse.core
import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}
import hedgehog._
import hedgehog.runner._


/**
 * @author Kevin Lee
 * @since 2017-07-22
 */
object EscapeParserSpec extends Properties {

  override def tests: List[Test] = List(
    example(
      """Parsers.escape.parse("'") return Failure(_, 0, _)""",
      testParsersEscapeParseOneSingleQuotes
    ),
    example(
      """Parsers.escape.parse("''") return Success("'", 2)""",
      testParsersEscapeParseTwoSingleQuotes
    ),
    example(
      """Parsers.escape.parse("\\") should return Failure(_, 0, _)""",
      testParsersEscapeParseDoubleBackslash
    ),
    property(
      raw"""Parsers.escape.parse(one of ${TestData.escapingCharsToString})""",
      testParsersEscapeParseOneOfEscapingChars
    )
  )

  def testParsersEscapeParseOneSingleQuotes: Result = {
    val actual = Parsers.escape.parse("'")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testParsersEscapeParseTwoSingleQuotes: Result = {
    val expected: Success[Any, Char, String] = Success("'", 2)
    val actual: Parsed[Any, Char, String] = Parsers.escape.parse("''")
    actual ==== expected
  }

  def testParsersEscapeParseDoubleBackslash: Result = {
    val actual = Parsers.escape.parse("\\")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testParsersEscapeParseOneOfEscapingChars: Property = for {
    value <- Gen.elementUnsafe(TestData.escapingChars).log("value")
  } yield {
    val expected: Success[Any, Char, String] = Success((), value.length)
    val actual: Parsed[Any, Char, String] = Parsers.escape.parse(value)

    val expectedCapture: core.Parsed[String, Char, String] = Success(value, value.length)
    import fastparse.all._
    val actualCaptured: core.Parsed[String, Char, String] = Parsers.escape.!.parse(value)
    actual ==== expected and actualCaptured ==== expectedCapture
  }

}
