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
        val actual = Parsers.number.parse("1234")
        actual should be (expected)
      }
    }

    "number.parse(non-negative int in String)" should {
      "return Success(int parsed, the length of the String)" in {
        forAll { (i: Int) =>
          whenever(i >= 0) {
            val input = i.toString
            val expected = Success(i, input.length)
            val actual = Parsers.number.parse(input)
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
          val actual = Parsers.number.parse(input)
          println(s"i: $i / actual: $actual / expected: $expected")
          actual should be (expected)
        }
      }
    }


  }
}
