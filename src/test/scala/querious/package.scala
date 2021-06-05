import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric._
import eu.timepit.refined.predicates.all._
import hedgehog.Result
import shapeless.{ ::, HNil }

/**
  * @author Kevin Lee
  * @since 2020-12-08
  */
package object querious {

  final case class LowerCaseAlphabet()
  object LowerCaseAlphabet {
    implicit val lowerCaseAlphabetValidate
      : Validate.Plain[Char, LowerCaseAlphabet] =
      Validate.fromPredicate(
        c => (c >= 'a' && c <= 'z'),
        t => s"isLowerCaseAlphabet('$t')",
        LowerCaseAlphabet()
      )

  }

  final case class UpperCaseAlphabet()
  object UpperCaseAlphabet {
    implicit val upperCaseAlphabetValidate
      : Validate.Plain[Char, UpperCaseAlphabet] =
      Validate.fromPredicate(
        c => (c >= 'A' && c <= 'Z'),
        t => s"isUpperCaseAlphabet('$t')",
        UpperCaseAlphabet()
      )

  }

  final case class Underscore()
  object Underscore {
    implicit val underscoreValidate: Validate.Plain[Char, Underscore] =
      Validate.fromPredicate(
        c => c == '_',
        t => s"isUnderscore$t')",
        Underscore()
      )
  }

  type AllDigit = boolean.And[NonEmpty, collection.Forall[Digit]]

  type AllLowerCase =
    boolean.And[NonEmpty, collection.Forall[LowerCaseAlphabet]]

  type AllUpperCase =
    boolean.And[NonEmpty, collection.Forall[UpperCaseAlphabet]]

  type AllAlphabets =
    boolean.And[NonEmpty, collection.Forall[boolean.Or[UpperCaseAlphabet, LowerCaseAlphabet]]]

  type NonAlphabets =
    boolean.Not[boolean.Or[collection.Exists[UpperCaseAlphabet],
                           collection.Exists[LowerCaseAlphabet]]]

  type PositiveInt = Int Refined Positive

  type DigitString = String Refined AllDigit

  type LowerCaseString = String Refined AllLowerCase

  type UpperCaseString = String Refined AllUpperCase

  type AlphabetString = String Refined AllAlphabets

  type NonAlphabetString = String Refined NonAlphabets

  type AlphaNum = boolean.And[NonEmpty, collection.Forall[boolean.AnyOf[UpperCaseAlphabet :: LowerCaseAlphabet :: Digit :: HNil]]]
  type AlphaNumString = String Refined AlphaNum

  type AlphaUnderscorePlusAlphaNumUnderscore =
    boolean.And[NonEmpty,
      boolean.And[collection.Head[boolean.AnyOf[UpperCaseAlphabet :: LowerCaseAlphabet :: Underscore :: HNil]], collection.Tail[boolean.AnyOf[UpperCaseAlphabet :: LowerCaseAlphabet :: Digit :: Underscore :: HNil]]]
    ]

  type AlphaUnderscorePlusAlphaNumUnderscoreString = String Refined AlphaUnderscorePlusAlphaNumUnderscore

  implicit final class PropertyOps[A](val a: A) extends AnyVal {
    def matchPattern(right: PartialFunction[Any, _]): Result =
      if (right.isDefinedAt(a)) Result.success
      else Result.failure.log(s"actual: $a")
  }

}
