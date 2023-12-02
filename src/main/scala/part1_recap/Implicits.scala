package part1_recap

import scala.util.Success

object Implicits {

  private case class Pet(name: String)

  case class Person(name: String) {
    def greet: String = s"Hello, $name!"
  }

  private case class Car(brand: String, model: String)

  // implicit classes
  implicit class PersonImplicits(name: String) {
    def asPerson = Person(name)
  }

  // import implicit conversions in scope
  import scala.concurrent.duration._
  1.second
  25.minutes

  // implicit arguments
  implicit val defaultMultiplier: Int = 10
  private def increase(x: Int)(implicit multiplier: Int) = x * multiplier

  // more complex example
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  private def toJson[T](list: List[T])(implicit
      jsonSerializer: JsonSerializer[T]
  ) =
    list.map(jsonSerializer.toJson).mkString("[", ",", "]")

  private def toJson[T <: Product](value: T)(implicit
      jsonSerializer: JsonSerializer[T]
  ) = jsonSerializer.toJson(value)

  implicit val personSerializer: JsonSerializer[Person] =
    (person: Person) => "My person name is " + person.name

  // implicit methods
  implicit def serializer1[T <: Product]: JsonSerializer[T] =
    (value: T) =>
      0.until(value.productArity)
        .map(it =>
          s""" "${value.productElementName(it)}":"${value.productElement(
            it
          )}" """.trim
        )
        .mkString("{", ",", "}")

  def main(args: Array[String]): Unit = {
    "Peter".asPerson

    increase(10)

    println(serializer1[Person].toJson(Person("Person")))

    println(toJson(List(Person("Person1"), "Person2".asPerson)))
    println(toJson(List(Pet("Pet1"), Pet("Pet2"))))
    println(toJson(List(Pet("Pet1"), Person("Person1"))))

    println(toJson(Person("Person")))
    println(toJson(Pet("Pet")))
    println(toJson(Car("Fiat", "Uno")))
  }

}
