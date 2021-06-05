package io.kevinlee.sql.parser

import eu.timepit.refined.auto._
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import hedgehog._
import hedgehog.runner._


/**
 * @author Kevin Lee
 * @since 2017-07-22
 */
object NumberPredicateParserSpec extends Properties {

  override def tests: List[Test] = List(
    property(
      """Parsers.numberPredicateParser.parse handles Eq so numberPredicateParser.parse("field = number value") return Success(Eq(field, number value), "field = number value".length)""",
      testParsersNumberPredicateParserParseEqCase
    ),
    property(
      """Parsers.numberPredicateParser.parse handles Ne so numberPredicateParser.parse("field != number value") return Success(Ne(field, number value), "field != number value".length)""",
      testParsersNumberPredicateParserParseNeCase
    ),
    property(
      """Parsers.numberPredicateParser.parse handles Le so numberPredicateParser.parse("field <= number value") return Success(Le(field, number value), "field <= number value".length)""",
      testParsersNumberPredicateParserParseLeCase
    ),
    property(
      """Parsers.numberPredicateParser.parse handles Lt so numberPredicateParser.parse("field < number value") return Success(Lt(field, number value), "field < number value".length)""",
      testParsersNumberPredicateParserParseLtCase
    ),
    property(
      """Parsers.numberPredicateParser.parse handles Ge so numberPredicateParser.parse("field >= number value") return Success(Ge(field, number value), "field >= number value".length)""",
      testParsersNumberPredicateParserParseGeCase
    ),
    property(
      """Parsers.numberPredicateParser.parse handles Gt so numberPredicateParser.parse("field > number value") return Success(Gt(field, number value), "field > number value".length)""",
      testParsersNumberPredicateParserParseGtCase
    )
  )

  def testParsersNumberPredicateParserParseEqCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue)).map(BigDecimal(_)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field = ${value.toString}"""
    val expected: Success[NumberPredicates.Eq, Char, String] = Success(NumberPredicates.Eq(field, value), input.length)
    val actual: Parsed[NumberPredicates.NumberPredicate, Char, String] = Parsers.numberPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersNumberPredicateParserParseNeCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue)).map(BigDecimal(_)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field != ${value.toString}"""
    val expected: Success[NumberPredicates.Ne, Char, String] = Success(NumberPredicates.Ne(field, value), input.length)
    val actual: Parsed[NumberPredicates.NumberPredicate, Char, String] = Parsers.numberPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersNumberPredicateParserParseLeCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue)).map(BigDecimal(_)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field <= ${value.toString}"""
    val expected: Success[NumberPredicates.Le, Char, String] = Success(NumberPredicates.Le(field, value), input.length)
    val actual: Parsed[NumberPredicates.NumberPredicate, Char, String] = Parsers.numberPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersNumberPredicateParserParseLtCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue)).map(BigDecimal(_)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field < ${value.toString}"""
    val expected: Success[NumberPredicates.Lt, Char, String] = Success(NumberPredicates.Lt(field, value), input.length)
    val actual: Parsed[NumberPredicates.NumberPredicate, Char, String] = Parsers.numberPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersNumberPredicateParserParseGeCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue)).map(BigDecimal(_)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field >= ${value.toString}"""
    val expected: Success[NumberPredicates.Ge, Char, String] = Success(NumberPredicates.Ge(field, value), input.length)
    val actual: Parsed[NumberPredicates.NumberPredicate, Char, String] = Parsers.numberPredicateParser.parse(input)
    actual ==== expected

  }

  def testParsersNumberPredicateParserParseGtCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue)).map(BigDecimal(_)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field > ${value.toString}"""
    val expected: Success[NumberPredicates.Gt, Char, String] = Success(NumberPredicates.Gt(field, value), input.length)
    val actual: Parsed[NumberPredicates.NumberPredicate, Char, String] = Parsers.numberPredicateParser.parse(input)
    actual ==== expected

  }

}
