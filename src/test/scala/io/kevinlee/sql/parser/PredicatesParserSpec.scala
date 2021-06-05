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
object PredicatesParserSpec extends Properties {

  override def tests: List[Test] = List(
    property(
      """Parsers.predicates handle Ne when predicates.parse("field = boolean value")
        |  should return Success(BooleanPredicates.Eq(field, boolean value), input.length)""".stripMargin,
      testParsersPredicatesHandleNe
    ),
    property(
      """Parsers.predicates handle Ne when predicates.parse("field != boolean value")
        |  should return Success(BooleanPredicates.Ne(field, boolean value), input.length)""".stripMargin,
      testParsersPredicatesHandleNe
    ),
    property(
      """Parsers.predicates handles Eq so predicates.parse("field = number value") return Success(NumberPredicates.Eq(field, number value), "field = number value".length)""",
      testParsersPredicatesParseNumberEqCase
    ),
    property(
      """Parsers.predicates handles Ne so predicates.parse("field != number value") return Success(NumberPredicates.Ne(field, number value), "field != number value".length)""",
      testParsersPredicatesParseNumberNeCase
    ),
    property(
      """Parsers.predicates handles Le so predicates.parse("field <= number value") return Success(NumberPredicates.Le(field, number value), "field <= number value".length)""",
      testParsersPredicatesParseNumberLeCase
    ),
    property(
      """Parsers.predicates handles Lt so predicates.parse("field < number value") return Success(NumberPredicates.Lt(field, number value), "field < number value".length)""",
      testParsersPredicatesParseNumberLtCase
    ),
    property(
      """Parsers.predicates handles Ge so predicates.parse("field >= number value") return Success(NumberPredicates.Ge(field, number value), "field >= number value".length)""",
      testParsersPredicatesParseNumberGeCase
    ),
    property(
      """Parsers.predicates handles Gt so predicates.parse("field > number value") return Success(NumberPredicates.Gt(field, number value), "field > number value".length)""",
      testParsersPredicatesParseNumberGtCase
    ),
    property(
      """Parsers.predicates handles Eq so predicates.parse("field = 'value'") should return Success(StringPredicates.Eq(field, value), "field != 'value'".length)""",
      testParsersPredicatesParseStringEqCase
    ).withTests(1000),
    property(
      """Parsers.predicates handles Ne so predicates.parse("field != 'value'") should return Success(StringPredicates.Ne(field, value), "field != 'value'".length)""",
      testParsersPredicatesParseStringNeCase
    ).withTests(1000),
    property(
      """Parsers.predicates handles Le so predicates.parse("field <= 'value'") should return Success(StringPredicates.Le(field, value), "field <= 'value'".length)""",
      testParsersPredicatesParseStringLeCase
    ).withTests(1000),
    property(
      """Parsers.predicates handles Lt so predicates.parse("field < 'value'") should return Success(StringPredicates.Lt(field, value), "field < 'value'".length)""",
      testParsersPredicatesParseStringLtCase
    ).withTests(1000),
    property(
      """Parsers.predicates handles Ge so predicates.parse("field >= 'value'") should return Success(StringPredicates.Ge(field, value), "field >= 'value'".length)""",
      testParsersPredicatesParseStringGeCase
    ).withTests(1000),
    property(
      """Parsers.predicates handles Gt so predicates.parse("field > 'value'") should return Success(StringPredicates.Gt(field, value), "field > '$value'".length)""",
      testParsersPredicatesParseStringGtCase
    ).withTests(1000)
  )

  def testParsersPredicatesHandleEq: Property =
    for {
      firstChar <- Gen
        .elementUnsafe(('_' +: (('a' to 'z') ++ ('A' to 'Z'))).toList)
        .log("firstChar")
      rest <- Gen
        .elementUnsafe(
          ('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toList)
        )
        .list(Range.linear(1, 10))
        .map(_.mkString)
        .log("rest")
      value <- Gen.boolean.log("value")

    } yield {

      val field = firstChar + rest
      val input = s"""$field = $value"""

      val expected: Success[BooleanPredicates.Eq, Char, String] =
        Success(BooleanPredicates.Eq(field, value), input.length)
      val actual: Parsed[Clause, Char, String] =
        Parsers.predicates.parse(input)

      actual ==== expected
    }

  def testParsersPredicatesHandleNe: Property =
    for {
      firstChar <- Gen
        .elementUnsafe(('_' +: (('a' to 'z') ++ ('A' to 'Z'))).toList)
        .log("firstChar")
      rest <- Gen
        .elementUnsafe(
          ('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toList)
        )
        .list(Range.linear(1, 10))
        .map(_.mkString)
        .log("rest")
      value <- Gen.boolean.log("value")

    } yield {

      val field = firstChar + rest
      val input = s"""$field != $value"""

      val expected: Parsed[BooleanPredicates.Ne, Char, String] =
        Success(BooleanPredicates.Ne(field, value), input.length)

      val actual: Parsed[Clause, Char, String] =
        Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseNumberEqCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .double(Range.linearFrac(Double.MinValue, Double.MaxValue))
        .map(BigDecimal(_))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field = ${value.toString}"""
      val expected: Success[NumberPredicates.Eq, Char, String] =
        Success(NumberPredicates.Eq(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseNumberNeCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .double(Range.linearFrac(Double.MinValue, Double.MaxValue))
        .map(BigDecimal(_))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field != ${value.toString}"""
      val expected: Success[NumberPredicates.Ne, Char, String] =
        Success(NumberPredicates.Ne(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseNumberLeCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .double(Range.linearFrac(Double.MinValue, Double.MaxValue))
        .map(BigDecimal(_))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field <= ${value.toString}"""
      val expected: Success[NumberPredicates.Le, Char, String] =
        Success(NumberPredicates.Le(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseNumberLtCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .double(Range.linearFrac(Double.MinValue, Double.MaxValue))
        .map(BigDecimal(_))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field < ${value.toString}"""
      val expected: Success[NumberPredicates.Lt, Char, String] =
        Success(NumberPredicates.Lt(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseNumberGeCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .double(Range.linearFrac(Double.MinValue, Double.MaxValue))
        .map(BigDecimal(_))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field >= ${value.toString}"""
      val expected: Success[NumberPredicates.Ge, Char, String] =
        Success(NumberPredicates.Ge(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected

    }

  def testParsersPredicatesParseNumberGtCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .double(Range.linearFrac(Double.MinValue, Double.MaxValue))
        .map(BigDecimal(_))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field > ${value.toString}"""
      val expected: Success[NumberPredicates.Gt, Char, String] =
        Success(NumberPredicates.Gt(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected

    }

  def testParsersPredicatesParseStringEqCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field = '$value'"""

      val expected: Success[StringPredicates.Eq, Char, String] =
        Success(StringPredicates.Eq(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseStringNeCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field != '$value'"""

      val expected: Success[StringPredicates.Ne, Char, String] =
        Success(StringPredicates.Ne(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseStringLeCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field <= '$value'"""

      val expected: Success[StringPredicates.Le, Char, String] =
        Success(StringPredicates.Le(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseStringLtCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field < '$value'"""

      val expected: Success[StringPredicates.Lt, Char, String] =
        Success(StringPredicates.Lt(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseStringGeCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field >= '$value'"""

      val expected: Success[StringPredicates.Ge, Char, String] =
        Success(StringPredicates.Ge(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

  def testParsersPredicatesParseStringGtCase: Property =
    for {
      fieldName <- Gens.genFieldName(10).log("fieldName")
      value <- Gen
        .string(Gens.genNonSingleQuoteNonBackslashChar, Range.linear(1, 30))
        .log("value")
    } yield {
      val field = fieldName.value
      val input = s"""$field > '$value'"""
      val expected: Success[StringPredicates.Gt, Char, String] =
        Success(StringPredicates.Gt(field, value), input.length)
      val actual: Parsed[Clause, Char, String] = Parsers.predicates.parse(input)
      actual ==== expected
    }

}
