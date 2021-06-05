package querious

import eu.timepit.refined.auto._
import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}
import hedgehog._
import hedgehog.runner._

/**
  * @author Kevin Lee
  * @since 2020-12-08
  */
object AlphabetParserSpec extends Properties {
  override def tests: List[Test] = List(
    property(
      "Parsers.AlphabetLower(lower case Char) should return true",
      testAlphabetLowerFunctionWithLowerCase
    ),
    property(
      "Parsers.AlphabetLower(upper case Char) should return false",
      testAlphabetLowerFunctionWithUpperCase
    ),
    property(
      "Parsers.AlphabetLower(digit Char) should return false",
      testAlphabetLowerFunctionWithDigit
    ),
    property(
      "Parsers.AlphabetLower(non lower case Char) should return false",
      testAlphabetLowerFunctionWithNonLowerCase
    ),
    property(
      "Parsers.AlphabetLower applied to lower case String.forall should return true",
      testAlphabetLowerFunctionWithLowerCaseStringForAll
    ),
    property(
      "Parsers.AlphabetUpper(upper case Char) should return true",
      testAlphabetUpperFunctionWithUpperCase
    ),
    property(
      "Parsers.AlphabetUpper(lower case Char) should return false",
      testAlphabetUpperFunctionWithLowerCase
    ),
    property(
      "Parsers.AlphabetUpper(digit Char) should return false",
      testAlphabetUpperFunctionWithDigit
    ),
    property(
      "Parsers.AlphabetUpper(non upper case Char) should return false",
      testAlphabetUpperFunctionWithNonUpperCase
    ),
    property(
      "Parsers.AlphabetUpper applied to upper case String.forall should return true",
      testAlphabetUpperFunctionWithUpperCaseStringForAll
    ),
    example(
      """Parsers.alphabetsLower.parse("") should return Failure""",
      testAlphabetsLowerParserWithEmptyString
    ),
    property(
      "Parsers.alphabetsLower.parse(alphabet lower case String) should return Success(parsed input, length)",
      testAlphabetsLowerParserWithLowerCaseString
    ),
    property(
      "Parsers.alphabetsLower.parse(alphabet upper case String) should return Failire(_, 0, _)",
      testAlphabetsLowerParserWithUpperCaseString
    ),
    property(
      "Parsers.alphabetsLower.parse(digit String) should return Failure(_, 0, _)",
      testAlphabetsLowerParserWithDigitString
    ),
    example(
      """Parsers.alphabetsUpper.parse("") should return Failure""",
      testAlphabetsUpperParserWithEmptyString
    ),
    property(
      "Parsers.alphabetsUpper.parse(alphabet upper case String) should return Success(parsed input, length)",
      testAlphabetsUpperParserWithUpperCaseString
    ),
    property(
      "Parsers.alphabetsUpper.parse(alphabet lower case String) should return Failire(_, 0, _)",
      testAlphabetsUpperParserWithLowerCaseString
    ),
    property(
      "Parsers.alphabetsUpper.parse(digit String) should return Failure(_, 0, _)",
      testAlphabetsUpperParserWithDigitString
    ),
    example(
      """Parsers.alphabets.parse("") should return Failure(_, 0, _)""",
      testAlphabetsParserWithEmptyString
    ),
    property(
      "Parsers.alphabets.parse(digit String) should return Failure(_, 0, _)",
      testAlphabetsParserWithDigitString
    ),
    property(
      "Parsers.alphabets.parse(upper case String) should return Success(parsed input, length)",
      testAlphabetsParserWithUpperCaseString
    ),
    property(
      "Parsers.alphabets.parse(lower case String) should return Success(parsed input, length)",
      testAlphabetsParserWithLowerCaseString
    ),
    property(
      "Parsers.alphabets.parse(alphabet String) should return Success(parsed input, length)",
      testAlphabetsParserWithAlphabetString
    ),
    property(
      "Parsers.alphabets.parse(non-alphabet String) should return Failure(_, 0, _)",
      testAlphabetsParserWithNonAlphabetString
    ),
    property(
      "Parsers.alphabets.parse(alphabet String part1 + one non-alphabet char + alphabet String part2) should return Success(part1, part1.length)",
      testAlphabetsParserWithAlphabetStringHavingOneNonAlphabetChar
    )
  )

  def testAlphabetLowerFunctionWithLowerCase: Property =
    for {
      c <- Gen.lower.log("c")
    } yield {
      val expected = true
      val actual = Parsers.AlphabetLower(c)
      actual ==== expected
    }

  def testAlphabetLowerFunctionWithUpperCase: Property =
    for {
      c <- Gen.upper.log("c")
    } yield {
      val expected = false
      val actual = Parsers.AlphabetLower(c)
      actual ==== expected
    }

  def testAlphabetLowerFunctionWithDigit: Property =
    for {
      c <- Gen.digit.log("c")
    } yield {
      val expected = false
      val actual = Parsers.AlphabetLower(c)
      actual ==== expected
    }

  def testAlphabetLowerFunctionWithNonLowerCase: Property =
    for {
      c <- Gen.unicode.map(x => if (x.isLower) (x + 'z').toChar else x).log("c")
    } yield {
      val expected = false
      val actual = Parsers.AlphabetLower(c)
      actual ==== expected
    }

  def testAlphabetLowerFunctionWithLowerCaseStringForAll: Property =
    for {
      s <- Gen.string(Gen.lower, Range.linear(1, 20)).log("s")
    } yield {
      val expected = true
      val actual = s.forall(Parsers.AlphabetLower)
      actual ==== expected
    }

  def testAlphabetUpperFunctionWithUpperCase: Property =
    for {
      c <- Gen.upper.log("c")
    } yield {
      val expected = true
      val actual = Parsers.AlphabetUpper(c)
      actual ==== expected
    }

  def testAlphabetUpperFunctionWithLowerCase: Property =
    for {
      c <- Gen.lower.log("c")
    } yield {
      val expected = false
      val actual = Parsers.AlphabetUpper(c)
      actual ==== expected
    }

  def testAlphabetUpperFunctionWithDigit: Property =
    for {
      c <- Gen.digit.log("c")
    } yield {
      val expected = false
      val actual = Parsers.AlphabetUpper(c)
      actual ==== expected
    }

  def testAlphabetUpperFunctionWithNonUpperCase: Property =
    for {
      c <- Gen.unicode.map(x => if (x.isUpper) (x + 'Z').toChar else x).log("c")
    } yield {
      val expected = false
      val actual = Parsers.AlphabetUpper(c)
      actual ==== expected
    }

  def testAlphabetUpperFunctionWithUpperCaseStringForAll: Property =
    for {
      s <- Gen.string(Gen.upper, Range.linear(1, 20)).log("s")
    } yield {
      val expected = true
      val actual = s.forall(Parsers.AlphabetUpper)
      actual ==== expected
    }

  def testAlphabetsLowerParserWithEmptyString: Result = {
    val actual = Parsers.alphabetsLower.parse("")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testAlphabetsLowerParserWithLowerCaseString: Property =
    for {
      input <- Gens.genNonEmptyLowerCaseString(10).log("input")
    } yield {
      val expected: Success[String, Char, String] = Success(input, input.length)
      val actual: Parsed[String, Char, String] =
        Parsers.alphabetsLower.parse(input)
      actual ==== expected
    }

  def testAlphabetsLowerParserWithUpperCaseString: Property =
    for {
      input <- Gens.genNonEmptyUpperCaseString(10).log("input")
    } yield {
      val actual = Parsers.alphabetsLower.parse(input)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testAlphabetsLowerParserWithDigitString: Property =
    for {
      input <- Gens.genNonEmptyDigitString(10).log("input")
    } yield {
      val actual = Parsers.alphabetsLower.parse(input)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testAlphabetsUpperParserWithEmptyString: Result = {
    val actual = Parsers.alphabetsUpper.parse("")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testAlphabetsUpperParserWithUpperCaseString: Property =
    for {
      input <- Gens.genNonEmptyUpperCaseString(10).log("input")
    } yield {
      val expected: Success[String, Char, String] = Success(input, input.length)
      val actual: Parsed[String, Char, String] =
        Parsers.alphabetsUpper.parse(input)
      actual ==== expected
    }

  def testAlphabetsUpperParserWithLowerCaseString: Property =
    for {
      input <- Gens.genNonEmptyLowerCaseString(10).log("input")
    } yield {
      val actual = Parsers.alphabetsUpper.parse(input)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testAlphabetsUpperParserWithDigitString: Property =
    for {
      input <- Gens.genNonEmptyDigitString(10).log("input")
    } yield {
      val actual = Parsers.alphabetsUpper.parse(input)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testAlphabetsParserWithEmptyString: Result = {
    val actual = Parsers.alphabets.parse("")
    actual matchPattern { case Failure(_, 0, _) => }
  }

  def testAlphabetsParserWithDigitString: Property =
    for {
      input <- Gens.genNonEmptyDigitString(10).log("input")
    } yield {
      val actual = Parsers.alphabets.parse(input)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testAlphabetsParserWithUpperCaseString: Property =
    for {
      input <- Gens.genNonEmptyUpperCaseString(10).log("input")
    } yield {
      val expected: Success[String, Char, String] = Success(input, input.length)
      val actual: Parsed[String, Char, String] = Parsers.alphabets.parse(input)
      actual ==== expected
    }

  def testAlphabetsParserWithLowerCaseString: Property =
    for {
      input <- Gens.genNonEmptyLowerCaseString(10).log("input")
    } yield {
      val expected: Success[String, Char, String] = Success(input, input.length)
      val actual: Parsed[String, Char, String] = Parsers.alphabets.parse(input)
      actual ==== expected
    }

  def testAlphabetsParserWithAlphabetString: Property =
    for {
      input <- Gens.genNonEmptyAlphabetString(10).log("input")
    } yield {
      val expectedValue = input.value
      val expected: Success[String, Char, String] = Success(expectedValue, expectedValue.length)
      val actual: Parsed[String, Char, String] = Parsers.alphabets.parse(input.value)
      actual ==== expected
    }

  def testAlphabetsParserWithNonAlphabetString: Property =
    for {
      input <- Gens.genNonEmptyNonAlphabetString(10).log("input")
    } yield {
      val actual: Parsed[String, Char, String] = Parsers.alphabets.parse(input)
      actual matchPattern { case Failure(_, 0, _) => }
    }

  def testAlphabetsParserWithAlphabetStringHavingOneNonAlphabetChar: Property =
    for {
      nonAlpha <- Gen
        .element1('\t', '\n', '\r', '#', '@', '$', '%', '*', '(', ')')
        .log("nonAlpha")
      part1 <- Gens.genNonEmptyAlphabetString(10).log("part1")
      part2 <- Gens.genNonEmptyAlphabetString(10).log("part2")
      input <- Gen
        .constant(part1.value + (nonAlpha +: part2.value.toList).mkString)
        .log("input")
    } yield {
      val actual: Parsed[String, Char, String] = Parsers.alphabets.parse(input)
      val expectedStringParsed = part1.value
      val expectedCount = expectedStringParsed.length
      (actual matchPattern {
        case Success(`expectedStringParsed`, `expectedCount`) =>
      }).log(
        s"expectedStringParsed: $expectedStringParsed, expectedCount: $expectedCount"
      )
    }

}
