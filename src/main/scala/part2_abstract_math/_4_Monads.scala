package part2_abstract_math

import java.util.concurrent.Executors
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
    def flat[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  import cats.instances.list._ // implicit Monad[List]
  import cats.instances.future._ // implicit Monad[Future]

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

  println("Specialized API:")
  println(pairLists(numbersList, charsList))
  println(pairOptions(numberOption, charOption))
  println(pairFutures(numberFuture, charFuture).value)

  // Generic API
  def pairs[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  println("Generic API:")
  println(pairs(numbersList, charsList))
  println(pairs(numberOption, charOption))
  println(pairs(numberFuture, charFuture))

}
