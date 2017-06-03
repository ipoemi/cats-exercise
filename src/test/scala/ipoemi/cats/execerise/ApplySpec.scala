package ipoemi.cats.execerise

import org.scalatest._

class ApplySpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  it should "provide map" in {
    val intToString: Int ⇒ String = _.toString
    val double: Int ⇒ Int = _ * 2
    val addTwo: Int ⇒ Int = _ + 2

    Apply[Option].map(Some(1))(intToString) should be(Some("1"))
    Apply[Option].map(Some(1))(double) should be(Some(2))
    Apply[Option].map(None)(addTwo) should be(None)
  }

  it should "provide compose" in {
    val listOpt = Apply[List] compose Apply[Option]
    val plusOne = (x: Int) ⇒ x + 1
    listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) should be(
      List(Some(2), None, Some(4))
    )
  }

}