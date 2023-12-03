package part1_intro

/** Type classes allow to extend or add functionality
  * to closed types (std library or 3th party libraries)
  * without extending its types
  */
class TypeClasses {

  case class Person(name: String, age: Int)

  // Part 1: type class definition
  trait XmlSerializer[T] {
    def toXml(value: T): String
  }

  // Part 2: implicit type class instances
  implicit object StringSerializer extends XmlSerializer[String] {
    override def toXml(value: String): String = s"<string>$value</string>"
  }

  implicit object IntSerializer extends XmlSerializer[Int] {
    override def toXml(value: Int): String = s"<int>$value</int>"
  }

  implicit object PersonSerializer extends XmlSerializer[Person] {
    override def toXml(value: Person): String =
      s"<person><name>" + value.name + "</name>" +
        "<age>" + value.age + "</age></person>"
  }

  // Part 3: offer some API

  def main(args: Array[String]): Unit = {}

}
