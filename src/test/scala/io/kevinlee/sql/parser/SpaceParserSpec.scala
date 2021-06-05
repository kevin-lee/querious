package io.kevinlee.sql.parser

import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}
import io.kevinlee.sql.parser.TestUtils._

import hedgehog._
import hedgehog.runner._

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
object SpaceParserSpec extends Properties {

  override def tests: List[Test] = List(
    example(
      raw"""Parsers.spaces.parse("") should return Failure(_, 0, _)""",
      testParsersSpacesParseEmptyString
    ),
    example(
      raw"""Parsers.spaces.parse(" ") should return Success((), 1) and Success(" ", 1)""",
      testParsersSpacesParseOneSpace
    ),
    example(
      raw"""Parsers.spaces.parse("\t") should return Success((), 1) and Success("\t", 1)""",
      testParsersSpacesParseBackslashT
    ),
    example(
      raw"""Parsers.spaces.parse("\n") should return Success((), 1) and Success("\n", 1)""",
      testParsersSpacesParseBackslashN
    ),
    example(
      raw"""Parsers.spaces.parse("\r\n") should return Success((), 2) and Success("\r\n", 2)""",
      testParsersSpacesParseBackslashRBackslashN
    ),
    example(
      raw"""Parsers.spaces.parse("\r") should return Success((), 1) and Success("\r", 1)""",
      testParsersSpacesParseBackslashR
    ),
    property(
      "Parsers.spaces.parse(nonWhitespace) should return Failure(_, 0, _)",
      testParsersSpacesParseNonWhitespace
    ),
    property(
      "Parsers.spaces.parse(whitespaces) should return Success((), whitespaces.length) and Success(whitespaces, whitespaces.length)",
      testParsersSpacesParseWhitespaces
    ),
    property(
      "Parsers.spaces.parse(whitespaces + nonWhitespace) should return Success((), whitespaces.length) and Success(whitespaces, whitespace.length)",
      testParsersSpacesParseWhitespaceAndNonWhitespace
    )
  )

  def testParsersSpacesParseEmptyString: Result = {
    val actual = Parsers.spaces.parse("")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testParsersSpacesParseOneSpace: Result = {
    val expected: Success[Unit, Char, String] = Success((), 1)
    val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse(" ")
    val expectedCapture: Success[String, Char, String] = Success(" ", 1)
    val actualString: Parsed[String, Char, String] =
      Parsers.spaces.parserOfString.parse(" ")
    actual ==== expected and actualString ==== expectedCapture
  }

  def testParsersSpacesParseBackslashT: Result = {
    val expected: Success[Unit, Char, String] = Success((), 1)
    val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse("\t")
    val expectedCapture: Success[String, Char, String] = Success("\t", 1)
    val actualString: Parsed[String, Char, String] =
      Parsers.spaces.parserOfString.parse("\t")
    actual ==== expected and actualString ==== expectedCapture
  }

  def testParsersSpacesParseBackslashN: Result = {
    val expected: Success[Unit, Char, String] = Success((), 1)
    val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse("\n")
    val expectedCapture: Success[String, Char, String] = Success("\n", 1)
    val actualString: Parsed[String, Char, String] =
      Parsers.spaces.parserOfString.parse("\n")
    actual ==== expected and actualString ==== expectedCapture
  }

  def testParsersSpacesParseBackslashR: Result = {
    val expected: Success[Unit, Char, String] = Success((), 1)
    val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse("\r")
    val expectedCapture: Success[String, Char, String] = Success("\r", 1)
    val actualString: Parsed[String, Char, String] =
      Parsers.spaces.parserOfString.parse("\r")
    actual ==== expected and actualString ==== expectedCapture
  }

  def testParsersSpacesParseBackslashRBackslashN: Result = {
    val expected: Success[Unit, Char, String] = Success((), 2)
    val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse("\r\n")
    val expectedCapture: Success[String, Char, String] = Success("\r\n", 2)
    val actualString: Parsed[String, Char, String] =
      Parsers.spaces.parserOfString.parse("\r\n")
    actual ==== expected and actualString ==== expectedCapture
  }

  def testParsersSpacesParseNonWhitespace: Property =
    for {
      nonWhitespace <- Gen
        .string(Gens.genNonWhitespaceUnicodeChar, Range.linear(1, 10))
        .log("nonWhitespace")
    } yield {
      val actual = Parsers.spaces.parse(nonWhitespace)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testParsersSpacesParseWhitespaces: Property =
    for {
      howMany <- Gen.int(Range.linear(1, 10)).log("howMany")
      whitespace <- Gens.genWhitespaceChar
        .list(Range.singleton(howMany))
        .map(_.mkString)
        .log("whitespace")
    } yield {
      val expected: Success[Unit, Char, String] = Success((), whitespace.length)
      val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse(whitespace)

      val expectedCapture: Success[String, Char, String] =
        Success(whitespace, whitespace.length)
      val actualString: Parsed[String, Char, String] =
        Parsers.spaces.parserOfString.parse(whitespace)

      actual ==== expected and actualString ==== expectedCapture
    }

  def testParsersSpacesParseWhitespaceAndNonWhitespace: Property =
    for {
      howMany <- Gen.int(Range.linear(1, 10)).log("howMany")
      whitespace <- Gens.genWhitespaceChar
        .list(Range.singleton(howMany))
        .map(_.mkString)
        .log("whitespace")
      nonWhitespace <- Gen
        .string(Gens.genNonWhitespaceUnicodeChar, Range.linear(1, 10))
        .log("nonWhitespace")
    } yield {
      val value = whitespace + nonWhitespace
      val expected: Success[Unit, Char, String] = Success((), whitespace.length)
      val actual: Parsed[Unit, Char, String] = Parsers.spaces.parse(value)

      val expectedCapture: Success[String, Char, String] =
        Success(whitespace, whitespace.length)
      val actualString: Parsed[String, Char, String] =
        Parsers.spaces.parserOfString.parse(value)

      actual ==== expected and actualString ==== expectedCapture
    }

}
