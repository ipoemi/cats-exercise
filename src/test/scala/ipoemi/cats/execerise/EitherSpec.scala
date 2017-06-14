package ipoemi.cats.execerise

import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {

  import cats.implicits._

  it should "be used to short-circuit a computation upon the first error" in {
    val right: Either[String, Int] = Either.right(5)
    right.map(_ + 1) should be(Either.right(6))

    val left: Either[String, Int] = Either.left("Something went wrong")
    left.map(_ + 1) should be(Either.left("Something went wrong"))
  }

  it should "provide flatMap method right-biased" in {
    val right: Either[String, Int] = Either.right(5)
    right.flatMap(x ⇒ Either.right(x + 1)) should be(Either.right(6))

    val left: Either[String, Int] = Either.left("Something went wrong")
    left.flatMap(x ⇒ Either.right(x + 1)) should be(Either.left("Something went wrong"))
  }

  object EitherStyle {
    def parse(s: String): Either[NumberFormatException, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

    def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
      if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Exception, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  it should "provide exception handling tools" in {
    EitherStyle.parse("Not a number").isRight should be(false)
    EitherStyle.parse("2").isRight should be(true)
  }

  it should "provide combinators like flatMap and map" in {
    import EitherStyle._

    magic("0").isRight should be(false)
    magic("1").isRight should be(true)
    magic("Not a number").isRight should be(false)
  }

  it should "provide pattern-matching" in {
    import EitherStyle._

    val result = magic("2") match {
      case Left(_: NumberFormatException) ⇒ "Not a number!"
      case Left(_: IllegalArgumentException) ⇒ "Can't take reciprocal of 0!"
      case Left(_) ⇒ "Unknown error"
      case Right(result) ⇒ s"Got reciprocal: ${result}"
    }
    result should be("Got reciprocal: 0.5")
  }

  object EitherStyleWithAdts {

    sealed abstract class Error

    sealed case class NotANumber(string: String) extends Error

    final case object NoZeroReciprocal extends Error

    def parse(s: String): Either[Error, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else Either.left(NotANumber(s))

    def reciprocal(i: Int): Either[Error, Double] =
      if (i == 0) Either.left(NoZeroReciprocal)
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Error, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  it should "provide pattern-matching with ADTs" in {
    import EitherStyleWithAdts._

    val result = magic("2") match {
      case Left(NotANumber(_)) ⇒ "Not a number!"
      case Left(NoZeroReciprocal) ⇒ "Can't take reciprocal of 0!"
      case Right(result) ⇒ s"Got reciprocal: ${result}"
    }
    result should be("Got reciprocal: 0.5")
  }

  it should "provide leftMap" in {
    val right: Either[String, Int] = Right(41)
    right.map(_ + 1) should be(Right(42))

    val left: Either[String, Int] = Left("Hello")
    left.map(_ + 1) should be(Left("Hello"))
    left.leftMap(_.reverse) should be(Left("olleH"))
  }

  it should "provide catchOnly, catchNonFatal" in {
    Either.catchOnly[NumberFormatException]("abc".toInt).isRight should be(false)

    Either.catchNonFatal(1 / 0).isLeft should be(true)
  }

  it should "provide additional syntax" in {
    val right: Either[String, Int] = 42.asRight[String]
    right should be(Either.right(42))
  }

}