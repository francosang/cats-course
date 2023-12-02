package part1_cats_intro

/**
 * Most important functionalities are type classes.
 * import cats.YourTypeClass           <- use your type class api
 * import cats.instances.yourType._    <- bring to scope implicit instances for your type
 * import cats.syntax.yourTypeClass._  <- use extension methods for the type class
 *
 * You can import all:
 * import cats._
 * import cats.implicits._
 */
object CatsOrganization {

  // 1: import the type class
  import cats.Eq

  // 2: import type class instances for each type
  import cats.instances.int._

  // 3: use the type class API
  Eq[Int] // summons the instance of the type class
  Eq[Int].eqv(2, 3)
  Eq[Int].eqv(2, 2)

  // 4: use syntax/extensions if available
  import cats.syntax.eq._
  2 === 3
  2 =!= 5
  2 === 2
  List(1) === List(1)

  // 5: create new custom instances for custom types
  case class ToyCar(name: String, price: BigInt)
  implicit val toyCar: Eq[ToyCar] =
    Eq.instance((a, b) => a.name === b.name && a.price === b.price)

  def main(args: Array[String]): Unit = {
    println(List(1) === List(1))
    println(ToyCar("Fiat", 10) === ToyCar("Fiat", 10))
    println(ToyCar("Fiat", 10) === ToyCar("Fiat", 20))
  }
}
