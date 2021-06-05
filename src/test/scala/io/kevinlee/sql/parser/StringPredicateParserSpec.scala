package io.kevinlee.sql.parser

import fastparse.core.Parsed
import fastparse.core.Parsed.Success

import eu.timepit.refined.auto._

import hedgehog._
import hedgehog.runner._


/**
 * @author Kevin Lee
 * @since 2017-07-22
 */
object StringPredicateParserSpec extends Properties {

  override def tests: List[Test] = List(
    property(
      """Parsers.stringPredicateParser handles Eq so stringPredicateParser.parse("field = 'value'") should return Success(StringPredicates.Eq(field, value), "field != 'value'".length)""",
      testParsersStringPredicateParserParseEqCase
    ).withTests(1000),
    property(
      """Parsers.stringPredicateParser handles Ne so stringPredicateParser.parse("field != 'value'") should return Success(StringPredicates.Ne(field, value), "field != 'value'".length)""",
      testParsersStringPredicateParserParseNeCase
    ).withTests(1000),
    property(
      """Parsers.stringPredicateParser handles Le so stringPredicateParser.parse("field <= 'value'") should return Success(StringPredicates.Le(field, value), "field <= 'value'".length)""",
      testParsersStringPredicateParserParseLeCase
    ).withTests(1000),
    property(
      """Parsers.stringPredicateParser handles Lt so stringPredicateParser.parse("field < 'value'") should return Success(StringPredicates.Lt(field, value), "field < 'value'".length)""",
      testParsersStringPredicateParserParseLtCase
    ).withTests(1000),
    property(
      """Parsers.stringPredicateParser handles Ge so stringPredicateParser.parse("field >= 'value'") should return Success(StringPredicates.Ge(field, value), "field >= 'value'".length)""",
      testParsersStringPredicateParserParseGeCase
    ).withTests(1000),
    property(
      """Parsers.stringPredicateParser handles Gt so stringPredicateParser.parse("field > 'value'") should return Success(StringPredicates.Gt(field, value), "field > '$value'".length)""",
      testParsersStringPredicateParserParseGtCase
    ).withTests(1000)
  )

  def testParsersStringPredicateParserParseEqCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field = '$value'"""

    val expected: Success[StringPredicates.Eq, Char, String] = Success(StringPredicates.Eq(field, value), input.length)
    val actual: Parsed[StringPredicates.StringPredicate, Char, String] = Parsers.stringPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersStringPredicateParserParseNeCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field != '$value'"""

    val expected: Success[StringPredicates.Ne, Char, String] = Success(StringPredicates.Ne(field, value), input.length)
    val actual: Parsed[StringPredicates.StringPredicate, Char, String] = Parsers.stringPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersStringPredicateParserParseLeCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field <= '$value'"""

    val expected: Success[StringPredicates.Le, Char, String] = Success(StringPredicates.Le(field, value), input.length)
    val actual: Parsed[StringPredicates.StringPredicate, Char, String] = Parsers.stringPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersStringPredicateParserParseLtCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field < '$value'"""

    val expected: Success[StringPredicates.Lt, Char, String] = Success(StringPredicates.Lt(field, value), input.length)
    val actual: Parsed[StringPredicates.StringPredicate, Char, String] = Parsers.stringPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersStringPredicateParserParseGeCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field >= '$value'"""

    val expected: Success[StringPredicates.Ge, Char, String] = Success(StringPredicates.Ge(field, value), input.length)
    val actual: Parsed[StringPredicates.StringPredicate, Char, String] = Parsers.stringPredicateParser.parse(input)
    actual ==== expected
  }

  def testParsersStringPredicateParserParseGtCase: Property = for {
    fieldName <- Gens.genFieldName(10).log("fieldName")
    value <- Gen.string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30)).log("value")
  } yield {
    val field = fieldName.value
    val input = s"""$field > '$value'"""
    val expected: Success[StringPredicates.Gt, Char, String] = Success(StringPredicates.Gt(field, value), input.length)
    val actual: Parsed[StringPredicates.StringPredicate, Char, String] = Parsers.stringPredicateParser.parse(input)
    actual ==== expected
  }

}
