package io.kevinlee.sql.parser

import fastparse.core.Parsed.Success
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

  "Parsers.alphabetLower" when {
    "Parsers.alphabetLower(lower case Char)" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.alphaLowerChar) { c =>
          whenever(Character.isLowerCase(c)) {
            val actual = Parsers.alphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.alphabetLower(upper case Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.alphaUpperChar) { c =>
          whenever(Character.isUpperCase(c)) {
            val actual = Parsers.alphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.alphabetLower(digit Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.numChar) { c =>
          whenever(Character.isDigit(c)) {
            val actual = Parsers.alphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "Parsers.alphabetLower(non lower case Char)" should {
      val expected = false
      s"return $expected" in {
        forAll(Gen.choose(1, 1000).map(_.toChar)) { c =>
          whenever(!Character.isLowerCase(c)) {
            val actual = Parsers.alphabetLower(c)
            actual should be (expected)
          }
        }
      }
    }
    "lower case alphabet String.forall(Parsers.alphabetLower)" should {
      val expected = true
      s"return $expected" in {
        forAll(Gen.alphaLowerStr) { value =>
          whenever(value.trim.nonEmpty && value.forall(Character.isLowerCase)) {
            val actual = value.forall(Parsers.alphabetLower)
            actual should be (expected)
          }
        }
      }
    }
  }
}
