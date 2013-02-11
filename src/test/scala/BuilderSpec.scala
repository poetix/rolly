package rolly

import org.specs2.Specification
import scalaz.Scalaz._
import scalaz._

import Builders._

class BuilderSpec extends Specification {

  case class Person(name: String, age: Int, address: Address)

  val name = Part[Person, String]("name")
  val age = Part[Person, Int]("age")
  val address = Part[Person, Address]("address")

  case class Address(lines: Seq[String], postcode: String)
  val lines = Part[Address, Seq[String]]("lines")
  val postcode = Part[Address, String]("postcode")


  implicit object PersonBuilder extends Builder[Person] {
    def build(parts: PartsList[Person]): Person = Person(name from parts, age from parts, address from parts)
  }

  implicit object AddressBuilder extends Builder[Address] {
    def build(parts: PartsList[Address]): Address = Address(lines from parts, postcode from parts)
  }

  def is =
  "A builder comprehension should" ^
    "build an object from parts" ! {
      val personBuilder = for {
        myAddress <- builder[Address]
        _         <- myAddress / lines := Seq("13 Valhalla Avenue")
        _         <- myAddress / postcode := "VB6 5UX"

        person  <- builder[Person]
        _       <- person / name := "Dominic"
        _       <- person / age  := 38
        _       <- person / address := myAddress
      } yield person

      buildFrom(personBuilder) must beEqualTo(Person("Dominic", 38, Address(Seq("13 Valhalla Avenue"), "VB6 5UX")))
    } ^
    "compose" ! {
      val partialPersonBuilder = for {
        person  <- builder[Person]
        _       <- person / name := "Dominic"
        _       <- person / age  := 38
      } yield person

      val personBuilder = for {
        myAddress <- builder[Address]
        person    <- partialPersonBuilder
        _         <- person / address := myAddress
        _         <- myAddress / lines := Seq("19 Acacia Avenue")
        _         <- myAddress / postcode := "LE1 1EL"
      } yield person

      buildFrom(personBuilder) must beEqualTo(Person("Dominic", 38, Address(Seq("19 Acacia Avenue"), "LE1 1EL")))
    } ^ end

}
