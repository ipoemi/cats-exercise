package ipoemi.cats.execerise

import org.scalatest._

class TraverseSpec extends FlatSpec with Matchers {

  import cats.Semigroup
  import cats.data.{NonEmptyList, OneAnd, Validated, ValidatedNel}
  import cats.implicits._

  def parseIntEither(s: String): Either[NumberFormatException, Int] =
    Either.catchOnly[NumberFormatException](s.toInt)

  def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
    Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel


  it should "provide effect" in {
    List("1", "2", "3").traverseU(parseIntEither) should be(Right(List(1, 2, 3)))
    List("1", "abc", "3").traverseU(parseIntEither).isLeft should be(true)
    List("1", "2", "3").traverseU(parseIntValidated).isValid should be(true)
  }

  it should "provide sequence" in {
    List(Option(1), Option(2), Option(3)).traverse(identity) should be(Option(List(1, 2, 3)))
    List(Option(1), None, Option(3)).traverse(identity) should be(None)
  }

  it should "provide sequence_" in {
    List(Option(1), Option(2), Option(3)).sequence_ should be(Option(Unit))
    List(Option(1), None, Option(3)).sequence_ should be(None)
  }

}