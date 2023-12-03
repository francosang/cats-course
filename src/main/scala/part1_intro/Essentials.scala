package part1_intro

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values
  val aBool: Boolean = true

  //expressions
  val anIf = if (aBool) "true" else "false"

  //instructions vs expressions
  val theUnit = println("Hello, scala. " + anIf)

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }
  class Lion extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = ???
  }

  // singleton
  object MySingleton

  // companions
  object Carnivore // companion object for the class Carnivore

  // Generics
  class Repository[A]

  // method notation
  val three = 1 + 3
  val four = 2.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(41)

  // high order functions: map, flatMap, filter, etc
  val processedList = List(1, 2, 3).map(incrementer)
  val longerList = List(1, 2, 3).flatMap(x => List(x, x + 1))

  // for-comprehensions
  val matrix = List(1, 2, 3).flatMap(n => List("a", "b", "c").map(c => (n, c)))
  val anotherMatrix = for {
    n <- List(1, 2, 3)
    c <- List("a", "b", "c")
  } yield (n, c)

  // options
  val anOption: Option[Int] = Option(3)
  val doubleOption: Option[Int] = anOption.map(_ * 2)

  // try
  val anAttempt = Try( /*something that can throw*/ 42) // Success(42)
  val modifiedAttempt = anAttempt.map(_ + 10) // Success(42)

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "infinite"
  }
  val optionContent = anOption match {
    case Some(value) => "The value is here" + value
    case None        => "the value is empty"
  }

  // Futures
//  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))

  val aFuture = Future {
    42
  }

  // wait for completion (async)
  aFuture.onComplete {
    case Failure(exception) => println(s"Error: $exception")
    case Success(value)     => println(s"Value is: $value")
  }
  println("This is printed first")

  // map a future
  aFuture.map(_ + 1).onComplete(it => "New val..." + it)
  aFuture
    .flatMap(res =>
      Future {
        res + 1
      }
    )
    .onComplete {
      case Failure(exception) => "Chain error: " + exception
      case Success(value)     => "Chain succeeded, value: " + value
    }
  println("This is printed second")

  // partial function
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1   => 13
    case 8   => 56
    case 100 => 998
  }

  aPartialFunction(1)
  aPartialFunction(8)
  aPartialFunction(100)

  // some more advanced stuff
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }
  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {
    println("Last?")

    Await.result(aFuture, Duration.Inf)
  }

}
