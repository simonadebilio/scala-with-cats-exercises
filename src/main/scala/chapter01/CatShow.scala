package chapter01

import cats._
import cats.implicits._

object CatShow {

  //  Re‐implement the Cat application from the previous section using Show instead of Printable.
  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show

    s"$name is a $age year-old $color cat."
  }
}
