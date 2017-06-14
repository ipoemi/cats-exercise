package ipoemi.cats.execerise

import org.scalatest._

class IdSpec extends FlatSpec with Matchers {

  import cats._

  it should "compare freely" in {
    val anId: Id[Int] = 42
    anId should be(42)
  }

  it should "provide pure" in {
    Applicative[Id].pure(42) should be(42)
  }

  it should "can be comonad: def coflatMap[A, B](a: Id[A])(f: Id[A] => B): Id[B] " in {
    val fortytwo: Int = 42
    Comonad[Id].coflatMap(fortytwo)(_ + 1) should be(43)
  }

}