package io.kevinlee.sql.parser

import fastparse.core.Parsed.{Failure, Success}
import io.kevinlee.sql.parser.TestUtils._
import io.kevinlee.sql.parser.StringInterpolation._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
class ParsersSpec extends WordSpec
                     with GeneratorDrivenPropertyChecks
                     with Matchers {

  val escapingChars = List("\\\"", "\\/", "\\\\", "\\b", "\\f", "\\n", "\\r", "\\t")

  "Parsers.`true`" when {
    """Parsers.true.parse("true")""" should {
      val expected = Success(true, 4)
      s"return $expected" in {
        val actual = Parsers.`true`.parse("true")
        actual should be (expected)
      }
    }
    """Parsers.true.parse("false")""" should {
      s"return Failure" in {
        val actual = Parsers.`true`.parse("false")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }
  }

  "Parsers.`false`" when {
    """Parsers.false.parse("false")""" should {
      val expected = Success(false, 5)
      s"return $expected" in {
        val actual = Parsers.`false`.parse("false")
        actual should be (expected)
      }
    }
    """Parsers.false.parse("true")""" should {
      s"return Failure" in {
        val actual = Parsers.`false`.parse("true")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }
  }

  "Parsers.AlphabetLower" when {
    "Parsers.AlphabetLower(lower case Char)" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.alphaLowerChar) { c =>
          whenever(Character.isLowerCase(c)) {
            val actual = Parsers.AlphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.AlphabetLower(upper case Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.alphaUpperChar) { c =>
          whenever(Character.isUpperCase(c)) {
            val actual = Parsers.AlphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.AlphabetLower(digit Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.numChar) { c =>
          whenever(Character.isDigit(c)) {
            val actual = Parsers.AlphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.AlphabetLower(non lower case Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.choose(1, 1000).map(_.toChar)) { c =>
          whenever(!Character.isLowerCase(c)) {
            val actual = Parsers.AlphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "lower case alphabet String.forall(Parsers.AlphabetLower)" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.trim.nonEmpty && value.forall(Character.isLowerCase)) {
            val actual = value.forall(Parsers.AlphabetLower)
            actual should be (expected)
          }
        }
      }
    }
  }


  "Given Parsers.AlphabetUpper" when {
    "Parsers.AlphabetUpper(upper case Char)" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.alphaUpperChar) { c =>
          whenever(Character.isUpperCase(c)) {
            val actual = Parsers.AlphabetUpper(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.AlphabetUpper(lower case Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.alphaLowerChar) { c =>
          whenever(Character.isLowerCase(c)) {
            val actual = Parsers.AlphabetUpper(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.AlphabetUpper(digit Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.numChar) { c =>
          whenever(Character.isDigit(c)) {
            val actual = Parsers.AlphabetUpper(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.AlphabetUpper(non upper case Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.choose(1, 1000).map(_.toChar)) { c =>
          whenever(!Character.isUpperCase(c)) {
            val actual = Parsers.AlphabetUpper(c)
            actual should be (expected)
          }
        }
      }
    }
    "upper case alphabet String.forall(Parsers.AlphabetUpper)" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.trim.nonEmpty && value.forall(Character.isUpperCase)) {
            val actual = value.forall(Parsers.AlphabetUpper)
            actual should be (expected)
          }
        }
      }
    }
  }

  "Parsers.Whitespace" when {
    val input = " \t\r\n"
    esc"""Parsers.Whitespace(one of "$input")""" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.oneOf(input)) { value =>
          whenever(input.contains(value)) {
            val actual = Parsers.Whitespace(value)
            actual should be(expected)
          }

        }
      }
    }
    esc"""Parsers.Whitespace(none of "$input")""" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.alphaNumChar) { value =>
          whenever(!input.contains(value)) {
            val actual = Parsers.Whitespace(value)
            actual should be(expected)
          }

        }
      }
    }
  }

  "Parsers.Digit" when {
    "Parsers.Digit(digit char)" should {
      val expected = true
      s"return $expected" in {

        forAll(Gen.numChar) { value =>
          whenever(value.isDigit) {
            val actual = Parsers.Digit(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.Digit(digit char)" should {
      val expected = false
      s"return $expected" in {

        forAll(Gen.alphaChar) { value =>
          whenever(!value.isDigit) {
            val actual = Parsers.Digit(value)
            actual should be (expected)
          }
        }

      }
    }

  }

  "Parsers.alphabetsLower" when {
    """Parsers.alphabetsLower("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.alphabetsLower.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.alphabetsLower(alphabet lower case String)""" should {
      "return Success(parsed value, length)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabetsLower.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    """Parsers.alphabetsLower(alphabet upper case String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val actual = Parsers.alphabetsLower.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

    """Parsers.alphabetsLower(digit String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isDigit)) {
            val actual = Parsers.alphabetsLower.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

  }

  "Parsers.alphabetsUpper" when {
    """Parsers.alphabetsUpper("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.alphabetsUpper.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.alphabetsUpper(alphabet upper case String)""" should {
      "return Success(parsed value, length)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabetsUpper.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    """Parsers.alphabetsUpper(alphabet lower case String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val actual = Parsers.alphabetsUpper.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

    """Parsers.alphabetsUpper(digit String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isDigit)) {
            val actual = Parsers.alphabetsUpper.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

  }

  "Parsers.alphabets" when {
    """Parsers.alphabets.parse("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.alphabets.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.alphabets.parse(some digit String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isDigit)) {
            val actual = Parsers.alphabets.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

    "Parsers.alphabets.parse(some uppercase alphabets)" should {
      "return Success(some uppercase alphabets, the length of the alphabets)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabets.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    "Parsers.alphabets.parse(some lowercase alphabets)" should {
      "return Success(some lowercase alphabets, the length of the alphabets)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabets.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    "Parsers.alphabets.parse(some alphabets)" should {
      "return Success(some alphabets, the length of the alphabets)" in {
        forAll(Gen.alphaStr) { value =>
          whenever(value.nonEmpty && value.forall(c => (c >= 65 && c <= 90) || (c >= 97 && c <= 122))) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabets.parse(value)
            actual should be (expected)
          }

        }
      }
    }

    """Parsers.alphabets.parse("øå")""" should {
      "return Failure(_, 0, _)" in {
            val value = "øå"
            val actual = Parsers.alphabets.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    val nonAlphaNumeric = Seq(' ', '\t', '\n', '\r', '#', '@', '$', '%', '*', '(', ')')
    s"""Parsers.alphabets.parse(one of $nonAlphaNumeric)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.listOf(Gen.oneOf(nonAlphaNumeric))) { value =>
          whenever(value.nonEmpty && value.forall(nonAlphaNumeric.contains(_))) {
            val actual = Parsers.alphabets.parse(value.mkString)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }
  }


  "Parsers.digits Spec" when {
    "digits.parse(\"1234\")" should {
      """return Success((), 4)""" in {
        val expected = Success((), 4)
        val actual = Parsers.digits.parse("1234")
        actual should be (expected)
      }
      """return Success("1234", 4) if captured""" in {
        val expected = Success("1234", 4)
        val actual = Parsers.digits.parserOfString.parse("1234")
        actual should be (expected)
      }
    }

    "digits.parse(\"\")" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.digits.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    "digits.parse(non-negative int in String)" should {
      "return Success((), the length of the String) and Success(int parsed in String, the length of the String) if captured" in {
        forAll { (i: Int) =>
          whenever(i >= 0) {
            val input = i.toString
            val expected = Success((), input.length)
            val actual = Parsers.digits.parse(input)
            actual should be (expected)

            val expectedCapture = Success(input, input.length)
            val actualCaptured = Parsers.digits.parserOfString.parse(input)
            actualCaptured should be (expectedCapture)
          }
        }
      }
    }

    "digits.parse(int between 0 and 10 in String)" should {
      "return Success((), the length of the String) and Success(int parsed in String, the length of the String) if captured" in {
        forAll(Gen.choose(1, 10)) { i =>
          val input = i.toString
          val expected = Success((), input.length)
          val actual = Parsers.digits.parse(input)
          actual should be (expected)
          val expectedCapture = Success(input, input.length)
          val actualCaptured = Parsers.digits.parserOfString.parse(input)
          actualCaptured should be (expectedCapture)
        }
      }
    }

  }


  "Parsers.alphaNumeric" when {
    """Parsers.alphaNumeric("")""" should {
      s"return Failure(_, 0, _)" in {
        val actual = Parsers.alphaNumerics.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    "Parsers.alphaNumerics(digit char)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.forall(_.isDigit) && value.nonEmpty) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumerics.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    "Parsers.alphaNumerics(digit String1 + non digit String + digit String2)" should {
      s"return Success(digit String1, digit String1 length)" in {
        forAll(Gen.numStr, Gen.numStr) { (value1: String, value2: String) =>
          whenever(value1.forall(_.isDigit) && value1.nonEmpty &&
            value2.forall(_.isDigit) && value2.nonEmpty) {
            val expected = Success(value1, value1.length)
            val actual = Parsers.alphaNumerics.parse(value1 + " " + value2)
            actual should be (expected)
          }
        }

      }
    }


    "Parsers.alphaNumerics(alphabet lower case char)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumerics.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    "Parsers.alphaNumerics(alphabet upper case char)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumerics.parse(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.alphaNumerics(alphabet lower + upper + digit String)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.alphaNumStr) { value =>
          whenever(value.nonEmpty &&
                   value.forall(x => x.isUpper || x.isLower || x.isDigit)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumerics.parse(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.alphaNumerics(alphabet lower + upper + digit String with non alpha numeric String)" should {
      s"return Success(only first valid alphanumeric String value, length)" in {
        val nonAlphaNumeric = Seq(' ', '#', '%')
        forAll(
          Gen.listOf(
            Gen.frequency((7, Gen.alphaNumChar),
                          (3, Gen.oneOf(nonAlphaNumeric))))
            .map(_.mkString)
        ) { value =>
          whenever(value.nonEmpty &&
                   value.forall(x => x.isUpper || x.isLower || x.isDigit || nonAlphaNumeric.mkString.contains(x))) {
            val nonAlphaNumericTaken = value.takeWhile(!nonAlphaNumeric.contains(_))

            val actual = Parsers.alphaNumerics.parse(value)
            if (nonAlphaNumericTaken.isEmpty) {
              actual should matchPattern { case Failure(_, 0, _) => }
            } else {
              val expected = Success(nonAlphaNumericTaken, nonAlphaNumericTaken.length)
              actual should be (expected)

            }

          }
        }

      }
    }

  }

  "Parsers.spaces" when {
    """Parsers.spaces.parse("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.spaces.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.spaces.parse(" ")""" should {
      val expected = Success((), 1)
      esc"""return $expected""" in {
        val actual = Parsers.spaces.parse(" ")
        actual should be (expected)
      }
      val expectedCapture = Success(" ", 1)
      esc"""return $expectedCapture if captured""" in {
        val actual = Parsers.spaces.parserOfString.parse(" ")
        actual should be (expectedCapture)
      }
    }

    """Parsers.spaces.parse("\t")""" should {
      val expected = Success((), 1)
      esc"""return $expected""" in {
        val actual = Parsers.spaces.parse("\t")
        actual should be (expected)
      }
      val expectedCapture = Success("\t", 1)
      esc"""return $expectedCapture if captured""" in {
        val actual = Parsers.spaces.parserOfString.parse("\t")
        actual should be (expectedCapture)
      }
    }

    """Parsers.spaces.parse("\n")""" should {
      val expected = Success((), 1)
      esc"""return $expected""" in {
        val actual = Parsers.spaces.parse("\n")
        actual should be (expected)
      }
      val expectedCapture = Success("\n", 1)
      esc"""return $expectedCapture if captured""" in {
        val actual = Parsers.spaces.parserOfString.parse("\n")
        actual should be (expectedCapture)
      }
    }

    """Parsers.spaces.parse("\r")""" should {
      val expected = Success((), 1)
      esc"""return $expected""" in {
        val actual = Parsers.spaces.parse("\r")
        actual should be (expected)
      }
      val expectedCapture = Success("\r", 1)
      esc"""return $expectedCapture if captured""" in {
        val actual = Parsers.spaces.parserOfString.parse("\r")
        actual should be (expectedCapture)
      }
    }

    forAll(Gen.alphaNumStr) { value =>
      whenever(value.nonEmpty && value.forall(!" \t\n\r".contains(_))) {
        raw"""Parsers.spaces.parse("$value")""" should {
          esc"""return Failure(_, 0, _)""" in {
            val actual = Parsers.spaces.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

    forAll(
      Gen.oneOf(Seq(' ', '\t', '\n', '\r')),
      Gen.alphaNumStr,
      Gen.choose(1, 10)
    ) { (whitespace: Char, nonWhiteSpace: String, howMany: Int) =>
      whenever(
        " \t\n\r".contains(whitespace) &&
        nonWhiteSpace.nonEmpty &&
        nonWhiteSpace.forall(!" \t\n\r".contains(_)) &&
        (howMany >= 1 && howMany <= 10)
      ) {
        val whitespaceString = whitespace.toString * howMany
        val value = whitespaceString + nonWhiteSpace
        raw"""Parsers.spaces.parse("$value")""" should {
          val expected = Success((), whitespaceString.length)
          esc"""return $expected""" in {
            val actual = Parsers.spaces.parse(value)
            actual should be (expected)
          }
          val expectedCapture = Success(whitespaceString, whitespaceString.length)
          esc"""return $expectedCapture""" in {
            val actual = Parsers.spaces.parserOfString.parse(value)
            actual should be (expectedCapture)
          }
        }
      }
    }

  }


  "Parsers.stringChars" when {
    """Parsers.stringChars.parse("''")""" should {
      """return Failure(_, 0, _)""" in {
        val actual = Parsers.stringChars.parse("''")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.stringChars.parse("\\")""" should {
      """return Failure(_, 0, _)""" in {
        val actual = Parsers.stringChars.parse("\\")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    forAll(Gen.alphaNumStr) { value =>
      whenever(value.nonEmpty &&
               value.forall(x => x.isUpper || x.isLower || x.isDigit)) {
        raw"""Parsers.stringChars.parse("$value")""" should {
          raw"""return Success($value, ${value.length})""" in {
            val expected = Success(value, value.length)
            val actual = Parsers.stringChars.parse(value)
            actual should be (expected)
          }
        }
      }
    }


    raw"""Parsers.stringChars.parse(one of ${escapingChars.map(x => esc"$x").mkString("[", ", ", "]")})""" should {
      raw"""return Failure(_, 0, _)""" in {
        forAll(Gen.oneOf(escapingChars)) { value =>
          whenever(escapingChars.contains(value)) {
            val actual = Parsers.stringChars.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

  }


  "Parsers.escape" when {
    """Parsers.escape.parse("'")""" should {
      """return Failure(_, 0, _)""" in {
        val actual = Parsers.escape.parse("'")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.escape.parse("''")""" should {
      """return Success("'", 2)""" in {
        val expected = Success("'", 2)
        val actual = Parsers.escape.parse("''")
        actual should be (expected)
      }
    }

    """Parsers.escape.parse("\\")""" should {
      """return Failure(_, 0, _)""" in {
        val actual = Parsers.escape.parse("\\")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    raw"""Parsers.escape.parse(one of ${escapingChars.map(x => esc"$x").mkString("[", ", ", "]")})""" should {
      raw"""return Success""" in {
        forAll(Gen.oneOf(escapingChars)) { value =>
          whenever(escapingChars.contains(value)) {
            val expected = Success((), value.length)
            val actual = Parsers.escape.parse(value)
            actual should be (expected)

            val expectedCapture = Success(value, value.length)
            val actualCaptured = Parsers.escape.parserOfString.parse(value)
            actualCaptured should be (expectedCapture)
          }
        }
      }
    }

  }

  "Parsers.strings" when {
    """Parsers.strings.parse("''")""" should {
      """return Success""" in {
        val expected = Success("", 2)
        val actual = Parsers.strings.parse("''")
        actual should be (expected)
      }
    }

    forAll(Gen.alphaNumStr) { value =>
      whenever(value.nonEmpty &&
        value.forall(x => x.isUpper || x.isLower || x.isDigit)) {
        raw"""Parsers.strings.parse("'$value'")""" should {
          raw"""return Success($value, ${value.length + 2})""" in {
            val expected = Success(value, value.length + 2)
            val actual = Parsers.strings.parse("'" + value + "'")
            actual should be(expected)
          }
        }
      }
    }

    """Parsers.strings.parse("''''")""" should {
      val expected = Success("'", 4)
      raw"""return $expected""" in {
        val actual = Parsers.strings.parse("''''")
        actual should be (expected)
      }
    }
    """Parsers.strings.parse("'abc''def'")""" should {
      val expectedString = "abc'def"
      val expected = Success(expectedString, expectedString.length + 3)
      raw"""return $expected""" in {
        val actual = Parsers.strings.parse("'abc''def'")
        actual should be (expected)
      }
    }

    """Parsers.strings.parse("'abc''''def'")""" should {
      val expectedString = "abc''def"
      val expected = Success(expectedString, expectedString.length + 4)
      raw"""return $expected""" in {
        val actual = Parsers.strings.parse("'abc''''def'")
        actual should be (expected)
      }
    }

    """Parsers.strings.parse("'abc''de''f'")""" should {
      val expectedString = "abc'de'f"
      val expected = Success(expectedString, expectedString.length + 4)
      raw"""return $expected""" in {
        val actual = Parsers.strings.parse("'abc''de''f'")
        actual should be (expected)
      }
    }

  }


  "Parsers.booleanPredicateParser" can {
    forAll(
      Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z'))),
      Gen.listOf(
        Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')))
      ).map(_.mkString),
      Gen.oneOf(true, false)) { (firstChar: Char, rest: String, value: Boolean) =>

      whenever(firstChar.isAllAlphaNumericOr('_') &&
               rest.isAllAlphaNumericOr('_')) {
        "handle Eq so" when {
          val field = firstChar + rest
          val input = s"""$field = $value"""

          s"""booleanPredicateParser.parse($input)""" should {
            val expected = Success(BooleanPredicates.Eq(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.booleanPredicateParser.parse(input)
              actual should be(expected)
            }

          }
        }
        "handle Ne so" when {
          val field = firstChar + rest
          val input = s"""$field != $value"""

          s"""booleanPredicateParser.parse($input)""" should {
            val expected = Success(BooleanPredicates.Ne(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.booleanPredicateParser.parse(input)
              actual should be(expected)
            }

          }
        }

      }

    }
  }


  "Parsers.numberPredicateParser" can {
    forAll(
      Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z'))),
      Gen.listOf(
        Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')))
      ).map(_.mkString),
      Gen.numStr) { (firstChar: Char, rest: String, valueString: String) =>

      whenever(firstChar.isAllAlphaNumericOr('_') &&
               rest.isAllAlphaNumericOr('_') &&
               valueString.isAllDigits) {
        val value = BigDecimal(valueString)

        "handle Eq so" when {
          val field = firstChar + rest
          val input = s"""$field = $value"""

          s"""numberPredicateParser.parse($input)""" should {
            val expected = Success(NumberPredicates.Eq(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.numberPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ne so" when {
          val field = firstChar + rest
          val input = s"""$field != $value"""

          s"""numberPredicateParser.parse($input)""" should {
            val expected = Success(NumberPredicates.Ne(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.numberPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Le so" when {
          val field = firstChar + rest
          val input = s"""$field <= $value"""

          s"""numberPredicateParser.parse($input)""" should {
            val expected = Success(NumberPredicates.Le(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.numberPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Lt so" when {
          val field = firstChar + rest
          val input = s"""$field < $value"""

          s"""numberPredicateParser.parse($input)""" should {
            val expected = Success(NumberPredicates.Lt(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.numberPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ge so" when {
          val field = firstChar + rest
          val input = s"""$field >= $value"""

          s"""numberPredicateParser.parse($input)""" should {
            val expected = Success(NumberPredicates.Ge(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.numberPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Gt so" when {
          val field = firstChar + rest
          val input = s"""$field > $value"""

          s"""numberPredicateParser.parse($input)""" should {
            val expected = Success(NumberPredicates.Gt(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.numberPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

      }

    }
  }


  "Parsers.stringPredicateParser" can {
    forAll(
      Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z'))),
      Gen.listOf(
        Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')))
      ).map(_.mkString),
      Gen.alphaNumStr) { (firstChar: Char, rest: String, value: String) =>

      whenever(firstChar.isAllAlphaNumericOr('_') &&
               rest.isAllAlphaNumericOr('_') &&
               value.isAllAlphaNumeric) {

        "handle Eq so" when {
          val field = firstChar + rest
          val input = s"""$field = '$value'"""

          s"""stringPredicateParser.parse($input)""" should {
            val expected = Success(StringPredicates.Eq(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.stringPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ne so" when {
          val field = firstChar + rest
          val input = s"""$field != '$value'"""

          s"""stringPredicateParser.parse($input)""" should {
            val expected = Success(StringPredicates.Ne(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.stringPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Le so" when {
          val field = firstChar + rest
          val input = s"""$field <= '$value'"""

          s"""stringPredicateParser.parse($input)""" should {
            val expected = Success(StringPredicates.Le(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.stringPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Lt so" when {
          val field = firstChar + rest
          val input = s"""$field < '$value'"""

          s"""stringPredicateParser.parse($input)""" should {
            val expected = Success(StringPredicates.Lt(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.stringPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ge so" when {
          val field = firstChar + rest
          val input = s"""$field >= '$value'"""

          s"""stringPredicateParser.parse($input)""" should {
            val expected = Success(StringPredicates.Ge(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.stringPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Gt so" when {
          val field = firstChar + rest
          val input = s"""$field > '$value'"""

          s"""stringPredicateParser.parse($input)""" should {
            val expected = Success(StringPredicates.Gt(field, value), input.length)
            s"return $expected" in {
              val actual = Parsers.stringPredicateParser.parse(input)
              actual should be(expected)
            }
          }
        }

      }

    }
  }


  "Parsers.predicates" can {
    forAll(
      Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z'))),
      Gen.listOf(
        Gen.oneOf('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')))
      ).map(_.mkString),
      Gen.oneOf(true, false),
      Gen.numStr,
      Gen.alphaNumStr) { (firstChar: Char,
                          rest: String,
                          booleanValue: Boolean,
                          numberValueString: String,
                          stringValue: String) =>

      whenever(firstChar.isAllAlphaNumericOr('_') &&
               rest.isAllAlphaNumericOr('_') &&
               numberValueString.isAllDigits &&
               stringValue.isAllAlphaNumeric) {
        "handle Eq so" when {
          val field = firstChar + rest
          val input = s"""$field = $booleanValue"""

          s"""predicates.parse($input)""" should {
            val expected = Success(BooleanPredicates.Eq(field, booleanValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }

          }
        }
        "handle Ne so" when {
          val field = firstChar + rest
          val input = s"""$field != $booleanValue"""

          s"""predicates.parse($input)""" should {
            val expected = Success(BooleanPredicates.Ne(field, booleanValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }

          }
        }

        val numberValue = BigDecimal(numberValueString)

        "handle Eq so" when {
          val field = firstChar + rest
          val input = s"""$field = $numberValueString"""

          s"""predicates.parse($input)""" should {
            val expected = Success(NumberPredicates.Eq(field, numberValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ne so" when {
          val field = firstChar + rest
          val input = s"""$field != $numberValueString"""

          s"""predicates.parse($input)""" should {
            val expected = Success(NumberPredicates.Ne(field, numberValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Le so" when {
          val field = firstChar + rest
          val input = s"""$field <= $numberValueString"""

          s"""predicates.parse($input)""" should {
            val expected = Success(NumberPredicates.Le(field, numberValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Lt so" when {
          val field = firstChar + rest
          val input = s"""$field < $numberValueString"""

          s"""predicates.parse($input)""" should {
            val expected = Success(NumberPredicates.Lt(field, numberValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ge so" when {
          val field = firstChar + rest
          val input = s"""$field >= $numberValueString"""

          s"""predicates.parse($input)""" should {
            val expected = Success(NumberPredicates.Ge(field, numberValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Gt so" when {
          val field = firstChar + rest
          val input = s"""$field > $numberValueString"""

          s"""predicates.parse($input)""" should {
            val expected = Success(NumberPredicates.Gt(field, numberValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }


        "handle Eq so" when {
          val field = firstChar + rest
          val input = s"""$field = '$stringValue'"""

          s"""predicates.parse($input)""" should {
            val expected = Success(StringPredicates.Eq(field, stringValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ne so" when {
          val field = firstChar + rest
          val input = s"""$field != '$stringValue'"""

          s"""predicates.parse($input)""" should {
            val expected = Success(StringPredicates.Ne(field, stringValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Le so" when {
          val field = firstChar + rest
          val input = s"""$field <= '$stringValue'"""

          s"""predicates.parse($input)""" should {
            val expected = Success(StringPredicates.Le(field, stringValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Lt so" when {
          val field = firstChar + rest
          val input = s"""$field < '$stringValue'"""

          s"""predicates.parse($input)""" should {
            val expected = Success(StringPredicates.Lt(field, stringValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Ge so" when {
          val field = firstChar + rest
          val input = s"""$field >= '$stringValue'"""

          s"""predicates.parse($input)""" should {
            val expected = Success(StringPredicates.Ge(field, stringValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

        "handle Gt so" when {
          val field = firstChar + rest
          val input = s"""$field > '$stringValue'"""

          s"""predicates.parse($input)""" should {
            val expected = Success(StringPredicates.Gt(field, stringValue), input.length)
            s"return $expected" in {
              val actual = Parsers.predicates.parse(input)
              actual should be(expected)
            }
          }
        }

      }

    }
  }

  val whereClauseData = List(
    "num = 999 OR abc != true OR name = 'Kevin'" ->
      Or(
        NumberPredicates.Eq("num", 999),
        Or(
          BooleanPredicates.Ne("abc", true),
          StringPredicates.Eq("name", "Kevin")
        )
      ),
    "num = 999 AND abc != true AND name = 'Kevin'" ->
      And(
        And(
          NumberPredicates.Eq("num", 999),
          BooleanPredicates.Ne("abc", true)
        ),
        StringPredicates.Eq("name", "Kevin")
      ),
    "num = 999 And abc != true OR name = 'Kevin'" ->
      Or(
        And(
          NumberPredicates.Eq("num", 999),
          BooleanPredicates.Ne("abc", true)
        ),
        StringPredicates.Eq("name", "Kevin")
      ),
    "num = 999 OR abc != true AND name = 'Kevin'" ->
      Or(
        NumberPredicates.Eq("num", 999),
        And(
          BooleanPredicates.Ne("abc", true),
          StringPredicates.Eq("name", "Kevin")
        )
      ),
    "num = 999 OR abc != true OR name = 'Kevin' AND price >= 50.50" ->
      Or(
        NumberPredicates.Eq("num", 999),
        Or(
          BooleanPredicates.Ne("abc", true),
          And(
            StringPredicates.Eq("name", "Kevin"),
            NumberPredicates.Ge("price", 50.50)
          )
        )
      ),
    "item_id =   1   AND   (  price > 100 OR c = true OR name = 'awesome product' AND (something <= 'blah' OR (a = true AND b != true  ) AND d < 12345))" ->
      And(
        NumberPredicates.Eq("item_id", 1),
        Or(
          NumberPredicates.Gt("price", 100),
          Or(
            BooleanPredicates.Eq("c", true),
            And(
              StringPredicates.Eq("name", "awesome product"),
              Or(
                StringPredicates.Le("something", "blah"),
                And(
                  And(
                    BooleanPredicates.Eq("a", true),
                    BooleanPredicates.Ne("b", true)
                  ),
                  NumberPredicates.Lt("d", 12345)
                )
              )
            )
          )
        )
      )
  )

  "Parsers.clause" when {
    for (
      (input, expectedClause) <- whereClauseData;
      expected = Success(expectedClause, input.length)) {
      s"clause.parse($input)" should {
        s"return $expected" in {
          val actual = Parsers.clause.parse(input)
          actual should be (expected)
        }
      }
    }
  }


  "Parsers.where" when {
    for (
      (whereClause, expectedClause) <- whereClauseData;
      input = s"WHERE $whereClause";
      expected = Success(expectedClause, input.length)) {
      s"where.parse($input)" should {
        s"return $expected" in {
          val actual = Parsers.where.parse(input)
          actual should be (expected)
        }
      }
    }
  }

}
