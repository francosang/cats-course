package part2_abstract_math

import cats.Functor

/** A Functor is a type class that provides a map method for any type.
  *
  * This is a higher kinded type class, which means that it
  * operates on type constructors rather than concrete types.
  *
  * Use cases: data structures meant to be transformed in sequence, to chain map calls
  */
object _3_Functor extends App {

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  trait Tree[+T] {}
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  // TODO 1: define a functor for a Tree[T]
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(value, left, right) =>
        Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  val tree: Tree[Int] = Branch(1, Leaf(2), Branch(3, Leaf(4), Leaf(5)))
  println(tree)

  import cats.syntax.functor._
  println(tree.map(_ * 2))

  val resTree = Functor[Tree].map[Int, Int](tree)(_ * 10)
  println(resTree)

  // TODO 2: define a short version of do10x
  def do10xShort[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

  // TODO 3: define a generic short version of do10x
  def mapTree[F[_]: Functor, I, R](container: F[I], f: I => R): F[R] =
    container.map(f)

  // TODO 4: define a extension of do10x
  implicit class TreeIntImplicits(tree: Tree[Int]) {
    def do10x: Tree[Int] = tree.map(_ * 10)
  }
  implicit class TreeImplicits[T](tree: Tree[T]) {
    def mapAll[I, R](f: T => R): Tree[R] = tree.map(f)
  }

  println(do10x(tree))
  println(do10xShort(tree))
  println(mapTree[Tree, Int, String](tree, it => s"==$it=="))
  println(tree.do10x)
  println(tree.mapAll(it => s"-->$it<--"))
}
