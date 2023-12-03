package part1_intro

/** Variance is a generic type annotation that allows
  * to propagate subtyping to the generic type.
  *
  * Variance affects hop type classes are fetched.
  *
  * Rule of Thumb:
  *  - HAS a T   = Covariant
  *  - ACTS on T = Contravariant
  *
  * Contravariant type classes can use superclass instances
  *  if specific instances are not available for that type
  *
  * Covariant type classes will always use the more specific type class instance
  *  but will confuse the compiler if more general type class is present
  *
  * Can't have Covariant and Contravariant
  *
  * Take away: always use general types and "smart" constructors
  */
object TypeClassAndVariance {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  // valid comparison
  Option(1) === Option(1)

  // invalid comparison
  //Some(2) === None --- Eq[Some[Int]] not found

  // variance
  class Animal
  case class Cat() extends Animal

  // Covariant type: subtyping is propagated to generic type
  class Cage[+T]
  val cage: Cage[Animal] =
    new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // Contravariant type: subtyping is propagated backwards from the generic type
  class Vet[-T]
  val vet: Vet[Cat] =
    new Vet[Animal] // Cat <: Animal, so Vet[Animal] <: Vet[Cat]

  // Contravariant type class
  trait SoundMaker[-T] {
    def makeSound(target: T): Unit
  }
  implicit object AnimalSoundMaker extends SoundMaker[Animal] {
    override def makeSound(target: Animal): Unit = println(
      s"Animal $target makes sound"
    )
  }
  def makeSound[T](target: T)(implicit soundMaker: SoundMaker[T]): Unit =
    soundMaker.makeSound(target)
  makeSound(new Animal) // ok - type clas instance defined above
  makeSound(new Cat) // also ok - type class for Animal also applicable to Cat

  // Rule 1: Contravariant type classes can use superclass instances
  // if specific instances are not available for that type

  // Has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]] {
    override def makeSound(target: Option[Int]): Unit = target match {
      case Some(_) => println("Option makes sound")
      case None    => println("Option does not exist")
    }
  }
  makeSound(Some(1))
  makeSound(None)
  makeSound(Option.apply(1))
  makeSound(Option.empty)

  // Covariant type class
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animals everywhere"
  }
  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "A lots of cats"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): Unit = println(event.show)
  organizeShow[Cat] // ok - the compile will inject CatShow as implicit
  // organizeShow[Animal] // fail - two instances available, the compiler is confuse

  // Rule 2: Covariant type classes will always use the more specific type class instance
  // for that type, but will confuse the compiler if more general type class is present

  // Rule 3: you can't have Covariant and Contravariant

  // Cats library uses Invariant type classes
  // Some(1) === None // fail -- Cats lib is invariant
  Option(1) === None
  Option(1) === Option.empty[Int]

  def main(args: Array[String]): Unit = {}

}
