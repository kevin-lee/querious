package io.kevinlee.sql.parser

import fastparse.core.Parsed.Success
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
      "return Success(1234, 4)" in {
        val expected = Success(1234, 4)
        val actual = Parsers.digit.parse("1234")
        actual should be (expected)
      }
    }

    "number.parse(non-negative int in String)" should {
      "return Success(int parsed, the length of the String)" in {
        forAll { (i: Int) =>
          whenever(i >= 0) {
            val input = i.toString
            val expected = Success(i, input.length)
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
          val expected = Success(i, input.length)
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


}
