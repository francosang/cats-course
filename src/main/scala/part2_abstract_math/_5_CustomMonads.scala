package part2_abstract_math

import scala.annotation.tailrec

object _5_CustomMonads extends App {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(a1)) => tailRecM(a1)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  // TODO: define monad for Identity[T]
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {

    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(
        f: A => Identity[B]
    ): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(
        f: A => Identity[Either[A, B]]
    ): Identity[B] = f(a) match {
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => value
    }
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](v: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(v)             => f(v)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???
  }

  val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
  val newTree: Tree[Int] =
    TreeMonad.flatMap(tree)(x => Branch(Leaf(x), Leaf(x + 1)))

  println(tree)
  println(newTree)

  val stringTree: Tree[String] =
    TreeMonad.map(tree)(x => s"Soy: $x !")

  println(stringTree)

}
