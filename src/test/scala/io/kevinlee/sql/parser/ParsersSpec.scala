package io.kevinlee.sql.parser

import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}
import StringInterpolation._

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
class ParsersSpec extends WordSpec
                     with GeneratorDrivenPropertyChecks
                     with Matchers {

  "Parsers.number Spec" when {
    "number.parse(\"1234\")" should {
      """return Success("1234", 4)""" in {
        val expected = Success("1234", 4)
        val actual = Parsers.digit.parse("1234")
        actual should be (expected)
      }
    }

    "number.parse(\"\")" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.digit.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    "number.parse(non-negative int in String)" should {
      "return Success(int parsed, the length of the String)" in {
        forAll { (i: Int) =>
          whenever(i >= 0) {
            val input = i.toString
            val expected = Success(input, input.length)
            val actual = Parsers.digit.parse(input)
            actual should be (expected)
          }
        }
      }
    }

    "number.parse(int between 0 and 10 in String)" should {
      "return Success(int parsed, the length of the String)" in {
        forAll(Gen.choose(1, 10)) { i =>
          val input = i.toString
          val expected = Success(input, input.length)
          val actual = Parsers.digit.parse(input)
          println(s"i: $i / actual: $actual / expected: $expected")
          actual should be (expected)
        }
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

  "Parsers.alphabetLower" when {
    """Parsers.alphabetLower("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.alphabetLower.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.alphabetLower(alphabet lower case String)""" should {
      "return Success(parsed value, length)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabetLower.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    """Parsers.alphabetLower(alphabet upper case String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val actual = Parsers.alphabetLower.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

    """Parsers.alphabetLower(digit String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isDigit)) {
            val actual = Parsers.alphabetLower.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

  }

  "Parsers.alphabetUpper" when {
    """Parsers.alphabetUpper("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.alphabetUpper.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.alphabetUpper(alphabet upper case String)""" should {
      "return Success(parsed value, length)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphabetUpper.parse(value)
            actual should be (expected)
          }
        }
      }
    }

    """Parsers.alphabetUpper(alphabet lower case String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val actual = Parsers.alphabetUpper.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

    """Parsers.alphabetUpper(digit String)""" should {
      "return Failure(_, 0, _)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isDigit)) {
            val actual = Parsers.alphabetUpper.parse(value)
            actual should matchPattern { case Failure(_, 0, _) => }
          }
        }
      }
    }

  }

  "Parsers.alphaNumeric" when {
    """Parsers.alphaNumeric("")""" should {
      s"return Failure(_, 0, _)" in {
        val actual = Parsers.alphaNumeric.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    "Parsers.alphaNumeric(digit char)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.numStr) { value =>
          whenever(value.forall(_.isDigit) && value.nonEmpty) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumeric.parse(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.alphaNumeric(digit String1 + non digit String + digit String2)" should {
      s"return Success(digit String1, digit String1 length)" in {
        forAll(Gen.numStr, Gen.numStr) { (value1: String, value2: String) =>
          whenever(value1.forall(_.isDigit) && value1.nonEmpty &&
            value2.forall(_.isDigit) && value2.nonEmpty) {
            val expected = Success(value1, value1.length)
            val actual = Parsers.alphaNumeric.parse(value1 + " " + value2)
            actual should be (expected)
          }
        }

      }
    }


    "Parsers.alphaNumeric(alphabet lower case char)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isLower)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumeric.parse(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.alphaNumeric(alphabet upper case char)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.alphaUpperStr) { value =>
          whenever(value.nonEmpty && value.forall(_.isUpper)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumeric.parse(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.alphaNumeric(alphabet lower + upper + digit String)" should {
      s"return Success(parsed value, length)" in {
        forAll(Gen.alphaNumStr) { value =>
          whenever(value.nonEmpty &&
                   value.forall(x => x.isUpper || x.isLower || x.isDigit)) {
            val expected = Success(value, value.length)
            val actual = Parsers.alphaNumeric.parse(value)
            actual should be (expected)
          }
        }

      }
    }

    "Parsers.alphaNumeric(alphabet lower + upper + digit String with non alpha numeric String)" should {
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

            val actual = Parsers.alphaNumeric.parse(value)
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

  "Parsers.space" when {
    """Parsers.space.parse("")""" should {
      "return Failure(_, 0, _)" in {
        val actual = Parsers.space.parse("")
        actual should matchPattern { case Failure(_, 0, _) => }
      }
    }

    """Parsers.space.parse(" ")""" should {
      val expected = Success(" ", 1)
      esc"""return $expected""" in {
        val actual = Parsers.space.parse(" ")
        actual should be (expected)
      }
    }

    """Parsers.space.parse("\t")""" should {
      val expected = Success("\t", 1)
      esc"""return $expected""" in {
        val actual = Parsers.space.parse("\t")
        actual should be (expected)
      }
    }

    """Parsers.space.parse("\n")""" should {
      val expected = Success("\n", 1)
      esc"""return $expected""" in {
        val actual = Parsers.space.parse("\n")
        actual should be (expected)
      }
    }

    """Parsers.space.parse("\r")""" should {
      val expected = Success("\r", 1)
      esc"""return $expected""" in {
        val actual = Parsers.space.parse("\r")
        actual should be (expected)
      }
    }

    forAll(Gen.alphaNumStr) { value =>
      whenever(value.nonEmpty && value.forall(!" \t\n\r".contains(_))) {
        raw"""Parsers.space.parse("$value")""" should {
          esc"""return Failure(_, 0, _)""" in {
            val actual = Parsers.space.parse(value)
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
        raw"""Parsers.space.parse("$value")""" should {
          val expected = Success(whitespaceString, whitespaceString.length)
          esc"""return $expected""" in {
            val actual = Parsers.space.parse(value)
            actual should be (expected)
          }
        }
      }
    }

  }
}
