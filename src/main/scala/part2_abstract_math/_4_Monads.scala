package part2_abstract_math

import cats.Monad

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/** A Monad is a type class which abstracts over sequencing computations.
  *
  * It is a higher kinded type class with two operations:
  * - pure (or return): wraps a value into a monadic context
  * - flatMap (or bind): sequences computations
  */
object _4_Monads extends App {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1: how do you create all combinations of (number, char) ?
  val combinationsFor: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)
  val combinations: List[(Int, Char)] =
    numbersList.flatMap(n => charsList.map(c => (n, c)))

  // ^ these combinations are identical

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  // TODO 1.1: how do you create all combinations of these options ?
  val combinationsOptionsFor: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)
  val combinationsOptions: Option[(Int, Char)] =
    numberOption.flatMap(n => charOption.map(c => (n, c)))

  // futures
  implicit val ec =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  val numberFuture = Future(42)
  val charFuture = Future('z')

  val combinationsFuturesFor: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)
  val combinationsFutures: Future[(Int, Char)] =
    numberFuture.flatMap(n => charFuture.map(c => (n, c)))

  /** Pattern
    *  - wrapping a value into a monadic value
    *  - the flatMap mechanism
    */

  trait CustomMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.future._
  import cats.instances.list._
  import cats.instances.option._ // implicit Monad[Future]

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption =
    optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  val listMonad = Monad[List]
  val aList = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  println("Cat monad instances:")
  println(anOption)
  println(aTransformedOption)
  println(aList)
  println(aTransformedList)
  println(aFuture)
  println(aTransformedFuture)

  // Specialized API
  def pairLists[A, B](listA: List[A], listB: List[B]): List[(A, B)] =
    listA.flatMap(a => listB.map(b => (a, b)))

  def pairOptions[A, B](
      optionA: Option[A],
      optionB: Option[B]
  ): Option[(A, B)] =
    optionA.flatMap(a => optionB.map(b => (a, b)))

  def pairFutures[A, B](
      futureA: Future[A],
      futureB: Future[B]
  ): Future[(A, B)] =
    futureA.flatMap(a => futureB.map(b => (a, b)))

  println("* Specialized API:")
  println(pairLists(numbersList, charsList))
  println(pairOptions(numberOption, charOption))
  println(pairFutures(numberFuture, charFuture).value)

  // Generic API
  def pairs[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // TODO: pairs with for-comprehensions
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def pairsFor[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    for {
      a <- ma
      two <- mb
    } yield (a, two)

  def pairsForWithScopedMonad[M[_]: Monad, A, B](
      ma: M[A],
      mb: M[B]
  ): M[(A, B)] =
    for {
      a <- ma
      two <- mb
    } yield (a, two)

  println("* Generic API:")
  println(pairs(numbersList, charsList))
  println(pairs(numberOption, charOption))
  println(pairs(numberFuture, charFuture))

  println("* Generic API using pairsFor:")
  println(pairsFor(numbersList, charsList))
  println(pairsFor(numberOption, charOption))
  println(pairsFor(numberFuture, charFuture))

  println("* Generic API using pairsForWithScopedMonad:")
  println(pairsForWithScopedMonad(numbersList, charsList))
  println(pairsForWithScopedMonad(numberOption, charOption))
  println(pairsForWithScopedMonad(numberFuture, charFuture))

  print(Extensions.transformedValue)

}

object Extensions {
  case class Box[A](value: A)

  // This is probably wrong, ignore.
  // Just assume there is a monad for Box existing
  implicit val boxMonad = new Monad[Box] {
    override def pure[A](x: A): Box[A] = Box(x)

    override def flatMap[A, B](fa: Box[A])(
        f: A => Box[B]
    ): Box[B] = f(fa.value)

    @tailrec
    override def tailRecM[A, B](
        a: A
    )(f: A => Box[Either[A, B]]): Box[B] = f(a) match {
      case Box(either) =>
        either match {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => Box(b)
        }
    }
  }

  /*
  Extension methods are provided in other packages
   */
  import cats.syntax.applicative._
  val oneValid: Box[Int] = 1.pure[Box]

  import cats.syntax.functor._
  val twoValid: Box[Int] = oneValid.map(_ + 1)

  import cats.syntax.flatMap._
  val transformedValue: Box[Int] = oneValid.flatMap(x => (x + 1).pure[Box])

  // The imports
}
