package io.kevinlee.sql.parser

import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.string._
import hedgehog._

/**
  * @author Kevin Lee
  * @since 2018-10-01
  */
object Gens {

  final val whitespaceChars =
    List(9, 10, 11, 12, 13, 28, 29, 30, 31, 32, 5760, 8192, 8193, 8194, 8195,
      8196, 8197, 8198, 8200, 8201, 8202, 8232, 8233, 8287, 12288)
  final val nonSingleQuoteNonBackslashCharRanges =
    List(0 -> 38, 40 -> 91, 93 -> 65535)

  def genNonWhitespaceUnicodeChar: Gen[Char] =
    Gen.frequencyUnsafe(Parsers.NonWhitespaceCharRange.map {
      case (from, to) =>
        (to + 1 - from, Gen.char(from.toChar, to.toChar))
    })

  def genString(maxLength: PositiveInt): Gen[String] =
    Gen.string(Gen.unicode, Range.linear(0, maxLength.value))

  def genNonEmptyDigitString(maxLength: PositiveInt): Gen[DigitString] =
    Gen
      .string(Gen.digit, Range.linear(1, maxLength.value))
      .map(a => refineV[AllDigit].unsafeFrom(a))

  def genNonEmptyLowerCaseString(maxLength: PositiveInt): Gen[LowerCaseString] =
    Gen
      .string(Gen.lower, Range.linear(1, maxLength.value))
      .map(a => refineV[AllLowerCase].unsafeFrom(a))

  def genNonEmptyUpperCaseString(maxLength: PositiveInt): Gen[UpperCaseString] =
    Gen
      .string(Gen.upper, Range.linear(1, maxLength.value))
      .map(a => refineV[AllUpperCase].unsafeFrom(a))

  def genNonEmptyAlphabetString(maxLength: PositiveInt): Gen[AlphabetString] =
    Gen
      .string(Gen.alpha, Range.linear(1, maxLength.value))
      .map(a => refineV[AllAlphabets].unsafeFrom(a))

  def genNonEmptyAlphaNumString(maxLength: PositiveInt): Gen[AlphaNumString] =
    Gen
      .string(Gen.alphaNum, Range.linear(1, maxLength.value))
      .map(a => refineV[AlphaNum].unsafeFrom(a))

  def genNonEmptyNonAlphabetString(
    maxLength: PositiveInt
  ): Gen[NonAlphabetString] =
    Gen
      .string(Gen.unicode.map { c =>
        if (c.isUpper || c.isLower) (c + 'A').toChar else c
      }, Range.linear(1, maxLength))
      .map(a => refineV[NonAlphabets].unsafeFrom(a))

  def genFieldName(
    maxLengthAfterFirstChar: PositiveInt
  ): Gen[AlphaUnderscorePlusAlphaNumUnderscoreString] =
    for {
      firstChar <- Gen.elementUnsafe(
        '_' :: (('a' to 'z') ++ ('A' to 'Z')).toList
      )
      rest <- Gen.string(
        Gen.elementUnsafe(
          '_' :: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toList
        ),
        Range.linear(1, maxLengthAfterFirstChar)
      )
    } yield
      refineV[AlphaUnderscorePlusAlphaNumUnderscore].unsafeFrom(
        firstChar + rest
      )

  def genWhitespaceChar: Gen[Char] =
    Gen.elementUnsafe(whitespaceChars).map(_.toChar)

  def genNonSingleQuoteNonBackslashChar: Gen[Char] =
    Gen.frequencyUnsafe(nonSingleQuoteNonBackslashCharRanges.map {
      case (start, end) =>
        (end + 1 - start) -> Gen.char(start.toChar, end.toChar)
    })

}
