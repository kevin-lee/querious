package io.kevinlee.sql.parser

import fastparse.core.Parsed.{Failure, Success}

import hedgehog._
import hedgehog.runner._

import eu.timepit.refined.auto._

/**
  * @author Kevin Lee
  * @since 2018-10-01
  */
object BooleanParserSpec extends Properties {
  def tests: List[Prop] =
    List(
      property("testTrueParser parsing non-boolean should fail", testTrueParser1),
      example("testTrueParser parsing true should succeed", testTrueParser2),
      example("testTrueParser parsing false should fail", testTrueParser3),
      property("testFalseParser parsing non-boolean should failure", testFalseParser1),
      example("testFalseParser parsing false should succeed", testFalseParser2),
      example("testFalseParser parsing true should fail", testFalseParser3)
    )

  def testTrueParser1: Property = for {
    a <- Gen.constant("a").log("a")
    rest <- Gens.genString(20).log("rest")
  } yield {
    Parsers.`true`.parse(a + rest) matchPattern { case Failure(_, 0, _) => }
  }

  def testTrueParser2: Result =
    Parsers.`true`.parse("true") matchPattern { case Success(true, _) => }

  def testTrueParser3: Result =
    Parsers.`true`.parse("false") matchPattern { case Failure(_, 0, _) => }

  def testFalseParser1: Property = for {
    a <- Gen.constant("a").log("a")
    rest <- Gens.genString(20).log("rest")
  } yield {
    Parsers.`false`.parse(a + rest) matchPattern { case Failure(_, 0, _) => }
  }

  def testFalseParser2: Result =
    Parsers.`false`.parse("false") matchPattern { case Success(false, _) => }

  def testFalseParser3: Result =
    Parsers.`false`.parse("true") matchPattern { case Failure(_, 0, _) => }

}
