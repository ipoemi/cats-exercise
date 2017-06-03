package ipoemi.cats.execerise

import org.scalatest._

class FunctorSpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  it should "provide map" in {
    Functor[Option].map(Option("Hello"))(_.length) should be(Option(5))
    Functor[Option].map(None: Option[String])(_.length) should be(None)
  }

  it should "provide lift" in {
    val lenOption: Option[String] ⇒ Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) should be(Some(5))
  }

  it should "provide fproduct" in {
    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    product.getOrElse("Cats", 0) should be(4)
    product.getOrElse("is", 0) should be(2)
    product.getOrElse("awesome", 0) should be(7)
  }

  it should "provide compose" in {
    val listOpt = Functor[List] compose Functor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be(
      List(Some(2), None, Some(4))
    )
  }

}