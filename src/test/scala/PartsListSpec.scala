package rolly

import org.specs2.Specification

class PartsListSpec extends Specification {

  val name = Part[String, String]("name")
  val age = Part[String, Int]("age")

  def is =
  "A parts list should" ^
    "be empty on initialisation" ! {
      PartsList.empty[String] must beEmpty
    } ^
    "be able to be extended with new parts" ! {
      val list = PartsList.empty[String] + (name of "Dominic") + (age of 38)

      list.parts must beEqualTo(Map(
        name -> "Dominic",
        age  -> 38
      ))
    } ^
    "be queryable for part values" ! {
      val list = PartsList.empty[String] + (name of "Dominic") + (age of 38)

      ((name from list) must beEqualTo("Dominic")) and
      ((age from list)  must beEqualTo(38))
    } ^ end
}
