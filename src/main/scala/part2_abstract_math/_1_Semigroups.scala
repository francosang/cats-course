package part2_abstract_math

/** Semigroups COMBINE elements of the same type into one element of that type.
  *
  * Combine many elements of a type without specifying how they should be combined.
  *
  * Combine (|+|) is always associative
  */
object Semigroups extends App {

  import cats.Semigroup

  private val intCombination =
    Semigroup[Int].combine(41, 1) // Addition for ints
  private val stringCombination =
    Semigroup[String].combine("Hello, ", "World.") // Concatenation for strings

  println(intCombination)
  println(stringCombination)

  // Combine many elements of a type without specifying how they should be combined
  // API for specific types
  private def reduceInts(ints: List[Int]): Int =
    ints.reduce(Semigroup[Int].combine)
  private def reduceStrings(ints: List[String]): String =
    ints.reduce(Semigroup[String].combine)
  // API for any type
  private def reduceThings[T](things: List[T])(implicit
      semigroup: Semigroup[T]
  ): T = things.reduce(semigroup.combine)

  val numbers = (1 to 10).toList

  // usage of specific API
  println(reduceInts(numbers))
  println(reduceStrings(List("I", " ", "like", " ", "semigroups")))

  // usage of generic API
  println(reduceThings(List(Option("Sca"), Some("la!"), None)))
  println(reduceThings(numbers.map(Option(_))))

  // TODO 1: define a combination for the type Expense
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] =
    Semigroup.instance { (a, b) =>
      Expense(
        id = if (a.amount >= b.amount) a.id else b.id,
        amount = Semigroup[Double].combine(a.amount, b.amount)
      )
    }
  println(reduceThings(List(Expense(1, 29.99), Expense(2, 0.99))))

  // Extension methods for semigroups
  import cats.syntax.semigroup._
  println(1 |+| 2)
  println("Name: " |+| "Pepe")
  println(Expense(1, 9.99) |+| Expense(2, 100.99))

  // TODO 2: implement another reduce with the combine syntax function |+|
  private def reduceThings2[T](things: List[T])(implicit
      semigroup: Semigroup[T]
  ): T = things.reduce(_ |+| _)

  private def reduceThings3[T: Semigroup](things: List[T]): T =
    things.reduce(_ |+| _)

}
