package io.kevinlee.sql.parser

import TestUtils._
import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}
import hedgehog._
import hedgehog.runner._

import eu.timepit.refined.auto._


/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
object NumberParserSpec extends Properties {

  override def tests: List[Test] = List(
    property(
      s"test digits between 0 and ${Int.MaxValue}",
      testParseDigitsBetweenZeroAndMax
    ),
    property(
      s"test digits between 0 and ${Int.MaxValue} (captured)",
      testParseDigitsBetweenZeroAndMaxCaptured
    ),
    property(
      s"test numbers between ${Long.MinValue} and ${Long.MaxValue}",
      testParseNumbersBetweenMinAndMax
    ),
    property(
      "Parsers.Digit(digit char) should return true",
      testParsersDigitWithDigitCharShouldReturnTrue
    ),
    property(
      "Parsers.Digit(non digit char) should return false",
      testParsersDigitWithNonDigitCharShouldReturnFalse
    ),
    property(
      "Parsers.digits.parse(digit String) should return Success((), the length of the String)",
      testParsersDigitsParseWithDigitStringShouldReturnSuccessWithLengthOfString
    ),
    property(
      "Parsers.digits.parserOfString.parse(digit String) should return Success(digit String, the length of the String)",
      testParsersDigitsParserOfStringParseWithDigitStringShouldReturnSuccessWithLengthOfString
    ),
    example(
      """Parsers.digits.parse("") should return Failure(_, 0, _)""",
      testParsersDigitsParseWithAnEmptyStringShouldReturnFailure
    ),
    property(
      "Parsers.digits.parse(non-negative int in String) should return Success((), the length of the String)",
      testParsersDigitsParseWithNonNegativeIntStringShouldReturnSuccessWithLengthOfString
    ),
    property(
      "Parsers.digits.parserOfString.parse(non-negative int in String) should return Success(int parsed in String, the length of the String)",
      testParsersDigitsParserOfStringParseWithNonNegativeIntStringShouldReturnSuccessWithStringAndLengthOfString
    )
  )

  def testParseDigitsBetweenZeroAndMax: Property =
    for {
      n <- Gen.int(Range.linear(0, Int.MaxValue)).log("n")
    } yield {
      val input = n.toString
      val expected: Parsed[Unit, Char, String] = Success((), input.length)
      val actual: Parsed[Unit, Char, String] = Parsers.digits.parse(input)
      actual ==== expected
    }

  def testParseDigitsBetweenZeroAndMaxCaptured: Property =
    for {
      n <- Gen.int(Range.linear(0, Int.MaxValue)).log("n")
    } yield {
      val input = n.toString
      val expected: Parsed[String, Char, String] = Success(input, input.length)
      val actual: Parsed[String, Char, String] =
        Parsers.digits.parserOfString.parse(input)
      actual ==== expected
    }

  def testParseNumbersBetweenMinAndMax: Property =
    for {
      n <- Gen.long(Range.linear(Long.MinValue, Long.MaxValue)).log("n")
    } yield {
      val input = n.toString
      val expected: Parsed[BigDecimal, Char, String] =
        Success(BigDecimal(input), input.length)
      val actual: Parsed[BigDecimal, Char, String] =
        Parsers.numbers.parse(input)
      actual ==== expected
    }

  def testParsersDigitWithDigitCharShouldReturnTrue: Property =
    for {
      n <- Gen.digit.log("n")
    } yield {
      val actual = Parsers.Digit(n)
      actual ==== true
    }

  def testParsersDigitWithNonDigitCharShouldReturnFalse: Property =
    for {
      c <- Gen.alpha.log("c")
    } yield {
      val actual = Parsers.Digit(c)
      actual ==== false
    }

  def testParsersDigitsParseWithDigitStringShouldReturnSuccessWithLengthOfString: Property = for {
    digitString <- Gens.genNonEmptyDigitString(20).log("digitString")
  } yield {
    val expected: Parsed[Unit, Char, String] = Success((), digitString.value.length)
    val actual = Parsers.digits.parse(digitString.value)
    actual ==== expected
  }

  def testParsersDigitsParserOfStringParseWithDigitStringShouldReturnSuccessWithLengthOfString: Property = for {
    digitString <- Gens.genNonEmptyDigitString(20).log("digitString")
  } yield {
    val expected: Parsed[String, Char, String] = Success(digitString.value, digitString.value.length)
    val actual = Parsers.digits.parserOfString.parse(digitString.value)
    actual ==== expected
  }

  def testParsersDigitsParseWithAnEmptyStringShouldReturnFailure: Result = {
    val actual = Parsers.digits.parse("")
    (actual matchPattern { case Failure(_, 0, _) => })
      .log(s"Expect Failure(_, 0, _) but got $actual instead.")
  }

  def testParsersDigitsParseWithNonNegativeIntStringShouldReturnSuccessWithLengthOfString: Property = for {
    nonNegativeInt <- Gen.int(Range.linear(0, Int.MaxValue)).log("nonNegativeInt")
  } yield {
    val input = nonNegativeInt.toString
    val expected: Parsed[Unit, Char, String] = Success((), input.length)
    val actual = Parsers.digits.parse(input)
    actual ==== expected
  }

  def testParsersDigitsParserOfStringParseWithNonNegativeIntStringShouldReturnSuccessWithStringAndLengthOfString: Property = for {
    nonNegativeInt <- Gen.int(Range.linear(0, Int.MaxValue)).log("nonNegativeInt")
  } yield {
    val input = nonNegativeInt.toString
    val expected: Parsed[String, Char, String] = Success(input, input.length)
    val actual = Parsers.digits.parserOfString.parse(input)
    actual ==== expected
  }


}
