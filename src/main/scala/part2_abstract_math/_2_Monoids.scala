package part2_abstract_math

/** Monoids are semigroup with a neutral value.
  *
  * They contain an empty element.
  */
object Monoids extends App {

  import cats.Semigroup
  import cats.syntax.semigroup._

  val numbers = (1 to 1000).toList
  val sumL = numbers.foldLeft(0)(_ |+| _)
  // starts with 0, sums from right to left
  val sumR = numbers.foldRight(0)(_ |+| _)

  val strings = List("Hello", " ", ",", "World", " ", "!")
  // starts with "", concatenates from left to right
  val concatL = strings.foldLeft("")(_ |+| _)
  // starts with "", concatenates from right to left
  val concatR = strings.foldRight("")(_ |+| _)

  // lets define a generic api for every type
  /*
  def combineLeft[T: Semigroup](list: List[T]): T =
    list.foldLeft( /* STARTING VALUE??? */ )(_ |+| _)
   */

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combinedInt = intMonoid.combine(1, 1) // 2
  val emptyInt = intMonoid.empty // 0

  val stringMonoid = Monoid[String]
  val combinedString = stringMonoid.combine("Sca", "la!") // "Scala!"
  val emptyString = stringMonoid.empty // ""

  val optionMonoid = Monoid[Option[Int]]
  val combinedOption = optionMonoid.combine(Option(1), Option(3)) // "Option(4)"
  val emptyOption = optionMonoid.empty // None

  // A generic API using monoids
  def combineLeft[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // TODO: implement a combine for Map[String, Int]
  val listOfMaps = List(
    Map("Ana" -> 1, "Bob" -> 2),
    Map("Charles" -> 3, "Dina" -> 4),
    Map("Ellie" -> 5, "Fred" -> 6)
  )

  def combineLeftMaps[K, V](list: List[Map[K, V]])(implicit
      monoid: Monoid[Map[K, V]]
  ) =
    list.foldLeft(monoid.empty)(_ |+| _)

  // TODO: implement a monoid to combine lists of ShoppingCarts
  case class ShoppingCart(items: List[String], total: Double)
  val listOfShoppingCarts: List[ShoppingCart] = List(
    ShoppingCart(List("iPhone 12", "Samsung Galaxy S21"), 2000),
    ShoppingCart(List("JBL Xtreme 2"), 150),
    ShoppingCart(List(), 0)
  )

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
      ShoppingCart(List(), 0),
      (sc1, sc2) =>
        ShoppingCart(sc1.items |+| sc2.items, sc1.total |+| sc2.total)
    )



  // Tests

  println(sumL)
  println(sumR)
  println(sumL == sumR)

  println(concatL)
  println(concatR)
  println(concatL == concatR)

  println(combinedInt)
  println(emptyInt)

  println(combinedString)
  println(emptyString)

  println(combinedOption)
  println(emptyOption)

  println(combineLeft(numbers))
  println(combineLeft(strings))
  println(combineLeft(List(Option(1), None, Option(2))))

  println(listOfMaps)
  println(combineLeftMaps(listOfMaps))
  println(combineLeft(listOfMaps))

  println(listOfShoppingCarts)
  println(combineLeft(listOfShoppingCarts))

}
