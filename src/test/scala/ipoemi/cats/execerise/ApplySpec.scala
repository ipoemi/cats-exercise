package ipoemi.cats.execerise

import org.scalatest._

class ApplySpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  val intToString: Int ⇒ String = _.toString
  val double: Int ⇒ Int = _ * 2
  val addTwo: Int ⇒ Int = _ + 2

  it should "provide map" in {
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

  it should "provide ap" in {
    Apply[Option].ap(Some(intToString))(Some(1)) should be(Some("1"))
    Apply[Option].ap(Some(double))(Some(1)) should be(Some(2))
    Apply[Option].ap(Some(double))(None) should be(None)
    Apply[Option].ap(None)(Some(1)) should be(None)
    Apply[Option].ap(None)(None) should be(None)
  }

  val addArity2 = (a: Int, b: Int) ⇒ a + b
  val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c

  it should "provide ap2, ap3, etc" in {
    Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) should be(Some(3))
    Apply[Option].ap2(Some(addArity2))(Some(1), None) should be(None)
    Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) should be(Some(6))
  }

  it should "provide map2, map3, etc" in {
    Apply[Option].map2(Some(1), Some(2))(addArity2) should be(Some(3))
    Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) should be(Some(6))
  }

  it should "provide tuple2, tuple3, etc" in {
    Apply[Option].tuple2(Some(1), Some(2)) should be(Some(1, 2))
    Apply[Option].tuple3(Some(1), Some(2), Some(3)) should be(Some(1, 2, 3))
  }

  it should "provide apply builder syntax" in {
    val option2 = Option(1) |@| Option(2)
    val option3 = option2 |@| Option.empty[Int]

    option2 map addArity2 should be( Some(3) )
    option3 map addArity3 should be( None )

    option2 apWith Some(addArity2) should be( Some(3) )
    option3 apWith Some(addArity3) should be( None )

    option2.tupled should be( Some(1, 2) )
    option3.tupled should be( None )
  }

}