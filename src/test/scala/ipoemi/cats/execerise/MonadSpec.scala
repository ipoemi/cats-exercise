package ipoemi.cats.execerise

import org.scalatest._

import scala.language.higherKinds

class MonadSpec extends FlatSpec with Matchers {

  import cats._
  import cats.implicits._

  it should "provide flatten" in {
    Option(Option(1)).flatten should be(Option(1))
    Option(None).flatten should be(None)
    List(List(1), List(2, 3)).flatten should be(List(1, 2, 3))
  }

  it should "provide pure" in {
    Monad[Option].pure(42) should be(Option(42))
  }

  it should "provide flatMap" in {
    Monad[List].flatMap(List(1, 2, 3))(x ⇒ List(x, x)) should be(List(1, 1, 2, 2, 3, 3))
  }

  it should "provide ifM" in {
    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be(Option("truthy"))
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4, 1, 2))
  }

  it should "provide Monad Transformer" in {
    case class OptionT[F[_], A](value: F[Option[A]])

    implicit def optionTMonad[F[_]](implicit F: Monad[F]) = {
      new Monad[({ type f[x] = OptionT[F, x]})#f] {
        def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))

        def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
          OptionT {
            F.flatMap(fa.value) {
              case None => F.pure(None)
              case Some(a) => f(a).value
            }
          }

        def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
          OptionT {
            F.tailRecM(a)(a0 => F.map(f(a0).value) {
              case None => Either.right[A, Option[B]](None)
              case Some(b0) => b0.map(Some(_))
            })
          }
      }
    }

    optionTMonad[List].pure(42) should be(OptionT(List(Option(42))))
  }

}