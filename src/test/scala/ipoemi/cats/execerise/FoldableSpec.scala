package ipoemi.cats.execerise

import org.scalatest._

class FoldableSpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  it should "provide foldLeft" in {
    Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) should be(6)
    Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) should be("abc")
  }

}