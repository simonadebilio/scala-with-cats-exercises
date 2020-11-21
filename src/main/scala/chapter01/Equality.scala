package chapter01

import cats._

object Equality {

  //  Implement an instance of Eq for our running Cat example:
  final case class Cat(name: String, age: Int, color: String)

  implicit val eqCat: Eq[Cat] =
    Eq.instance[Cat]((c1, c2) => c1.name == c2.name && c1.age == c2.age && c1.color == c2.color)
}
