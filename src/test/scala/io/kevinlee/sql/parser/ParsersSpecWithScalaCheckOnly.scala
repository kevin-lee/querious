package io.kevinlee.sql.parser

import fastparse.core.Parsed.Success
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.{Gen, Properties}

/**
  * @author Kevin Lee
  * @since 2017-07-22
  */
object ParsersSpecWithScalaCheckOnly extends Properties("String") {

  property("test numbers between 1 and 10") = forAll { (i: Int) =>
    (i >= 0) ==> {
      val input = i.toString
      val expected = Success(input, input.length)
      val actual = Parsers.digit.parse(input)
      actual == expected
    }
  }

  property("test numbers between 1 and 10 (using choose)") =
    forAll(Gen.choose(1, 10)) { i =>
      val input = i.toString
      val expected = Success(input, input.length)
      val actual = Parsers.digit.parse(input)
      actual == expected
    }
}
