package querious

import hedgehog._
import hedgehog.runner._

/**
 * @author Kevin Lee
 * @since 2020-12-20
 */
object WhitespaceParserSpec extends Properties {

  final val whitespaces: Seq[Char] = Parsers.WhitespaceChars

  override def tests: List[Test] = List(
    example("test Parsers.Whitespace(whiteshpace char) should return true", testWhitespaceParserWithWhitespace),
    example("test Parsers.Whitespace(non whiteshpace char) should return false", testWhitespaceParserWithNonWhitespace)
  )

  def testWhitespaceParserWithWhitespace: Result = {
    val actual = whitespaces.forall(Parsers.Whitespace)
    actual ==== true
  }

  def testWhitespaceParserWithNonWhitespace: Result = {
    val nonWhitespace = Parsers.NonWhitespaceCharRange.flatMap { case (from, to) =>
      (from to to).map(_.toChar).toList
    }
    val actual = nonWhitespace.exists(Parsers.Whitespace)
    actual ==== false
  }

}
