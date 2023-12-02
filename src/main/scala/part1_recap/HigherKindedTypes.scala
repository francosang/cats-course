package part1_recap

import part1_recap.HigherKindedTypes.Combinator

import scala.concurrent.Future
import scala.util.Try

/** Are generic types where their type arguments itself contain a generic type.
  *
  * Useful for abstracting a library, e.g. Cats
  */
object HigherKindedTypes {

  class Wrapper[F[_]]
  class Wrapper2[F[_], G[_], A]

  val example = new Wrapper[List]
  val example2 = new Wrapper2[Future, Option, String]

  // Exercise:
  //  - Generalize these methods and avoid duplication
  def combine_[A, B](a: List[A], b: List[B]): List[(A, B)] = for {
    a <- a
    b <- b
  } yield (a, b)

  def combine_[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = for {
    a <- a
    b <- b
  } yield (a, b)

  def combine_[A, B](a: Try[A], b: Try[B]): Try[(A, B)] = for {
    a <- a
    b <- b
  } yield (a, b)

  // 1: Define Type Class
  trait Combinator[F[_]] {
    def combine[A, B](a: F[A], b: F[B]): F[(A, B)]
  }

  // 2: Type Class instances
  implicit val listCombinator: Combinator[List] = new Combinator[List] {
    override def combine[A, B](a: List[A], b: List[B]): List[(A, B)] = for {
      a <- a
      b <- b
    } yield (a, b)
  }

  implicit val optionCombinator: Combinator[Option] = new Combinator[Option] {
    override def combine[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
      for {
        a <- a
        b <- b
      } yield (a, b)
  }

  implicit val tryCombinator: Combinator[Try] = new Combinator[Try] {
    override def combine[A, B](a: Try[A], b: Try[B]): Try[(A, B)] = for {
      a <- a
      b <- b
    } yield (a, b)
  }

  case class Box[F](value: F)

  implicit val boxCombinator: Combinator[Box] = new Combinator[Box] {
    override def combine[A, B](a: Box[A], b: Box[B]): Box[(A, B)] =
      Box((a.value, b.value))
  }

  // 3: API
  private def combine[F[_], A, B](a: F[A], b: F[B])(implicit
      combinator: Combinator[F]
  ): F[(A, B)] =
    combinator.combine(a, b)

  def main(args: Array[String]): Unit = {
    combine(List(1, 2, 3), List("1", "2", "3"))
    combine(Option(1), Option("1"))
    combine(Try(1), Try("1"))
    combine(Box(1), Box("1"))
  }

}
