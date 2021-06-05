package querious

/**
  * @author Kevin Lee
  * @since 2017-12-31
  */
object TestUtils {
  implicit class ParserOfUnitToParserOfString(val p: fastparse.all.P[Unit]) extends AnyVal {
    import fastparse.all._
    def parserOfString: P[String] = p.!
  }

  implicit class ParserOfAnyToParserOfString(val p: fastparse.all.P[Any]) extends AnyVal {
    import fastparse.all._
    def parserOfString: P[String] = p.!
  }

  def isAllAlphabets(value: String): Boolean =
    value.nonEmpty && value.forall(c =>  c.isLower || c.isUpper)

  def isAllAlphaNumeric(value: String): Boolean =
    value.nonEmpty && value.forall(c =>  c.isLower || c.isUpper || c.isDigit)

  def isAllAlphaNumericOr(x: Char)(value: String): Boolean =
    value.nonEmpty && value.forall(c =>  c.isLower || c.isUpper || c.isDigit || c == x)

  def isAllDigits(value: String): Boolean = value.forall(_.isDigit)

  implicit class StringValidator(val value: String) extends AnyVal {
    def isAllAlphabets: Boolean = TestUtils.isAllAlphabets(value)

    def isAllAlphaNumeric: Boolean = TestUtils.isAllAlphaNumeric(value)

    def isAllAlphaNumericOr(x: Char): Boolean = TestUtils.isAllAlphaNumericOr(x)(value)

    def isAllDigits: Boolean = TestUtils.isAllDigits(value)
  }


  def isAlphabet(c: Char): Boolean = c.isLower || c.isUpper

  def isAlphaNumeric(c: Char): Boolean = c.isLower || c.isUpper || c.isDigit

  def isAlphaNumericOr(x: Char)(c: Char): Boolean =
    c.isLower || c.isUpper || c.isDigit || c == x

  implicit class CharValidator(val c: Char) extends AnyVal {
    def isAllAlphabets: Boolean = TestUtils.isAlphabet(c)

    def isAllAlphaNumeric: Boolean = TestUtils.isAlphaNumeric(c)

    def isAllAlphaNumericOr(x: Char): Boolean = TestUtils.isAlphaNumericOr(x)(c)
  }

}
