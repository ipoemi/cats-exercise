package ipoemi.cats.execerise

import org.scalatest._

class MonoidSpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  it should "provide empty, combine" in {
    Monoid[String].empty should be("")
    Monoid[String].combineAll(List("a", "b", "c")) should be("abc")
    Monoid[String].combineAll(List()) should be("")
  }

  it should "provide combineAll" in {
    Monoid[Map[String, Int]].combineAll(List(Map("a" → 1, "b" → 2), Map("a" → 3))) should be {
      Map("a" -> 4, "b" -> 2)
    }
    Monoid[Map[String, Int]].combineAll(List()) should be {
      Map()
    }
  }

  it should "provide foldMap" in {
    val l = List(1, 2, 3, 4, 5)
    l.foldMap(identity) should be(15)
    l.foldMap(i ⇒ i.toString) should be("12345")
    l.foldMap(i ⇒ (i, i.toString)) should be((15, "12345"))
  }

}