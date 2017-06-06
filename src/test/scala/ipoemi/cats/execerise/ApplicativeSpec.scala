package ipoemi.cats.execerise

import org.scalatest._

class ApplicativeSpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  it should "provide pure" in {
    Applicative[Option].pure(1) should be(Option(1))
    Applicative[List].pure(1) should be(List(1))
    (Applicative[List] compose Applicative[Option]).pure(1) should be(List(Option(1)))
    Monad[Option].pure(1) should be(Option(1))
    Applicative[Option].pure(1) should be(Option(1))
  }

}