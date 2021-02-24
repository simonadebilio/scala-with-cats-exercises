package chapter01

import chapter01.Printable.format

// Define a type class Printable[A] containing a single method format.
// format should accept a value of type A and return a String .
trait Printable[A] {
  def format(a: A): String
}

// Create an object PrintableInstances containing instances of
// Printable for String and Int.
object PrintableInstances {
  implicit val printableString: Printable[String] = new Printable[String] {
    override def format(a: String): String = a
  }

  implicit val printableInt: Printable[Int] = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }
}

// Define an object Printable with two generic interface methods:
//  - format accepts a value of type A and a Printable of the corresponding type.
//    It uses the relevant Printable to convert the A to a String.
//  - print accepts the same parameters as format and returns Unit.
//    It prints the formatted A value to the console using println.
object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String =
    printable.format(a)

  def print[A](a: A)(implicit printable: Printable[A]): Unit =
    println(format(a))
}

/*
The code above forms a general purpose printing library that we can use in multiple applications.
Let’s define an “application” now that uses the library.
First we’ll define a data type to represent a well‐known type of furry animal:
 */
final case class Cat(name: String, age: Int, color: String)

/*
Next we’ll create an implementation of Printable for Cat that returns content in the following format:
NAME is a AGE year-old COLOR cat.
 */
object Cat {
//  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
//    override def format(c: Cat): String = s"${c.name} is a ${c.age} year-old ${c.color} cat."
//  }

  import PrintableInstances._
  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(c: Cat): String = {
      val catName: String = Printable.format(c.name)
      val catAge: String = Printable.format(c.age)
      val catColor: String = Printable.format(c.color)

      s"$catName is a $catAge year-old $catColor cat."
    }
  }

  def main(args: Array[String]): Unit = {
    val fish: Cat = Cat("Fish", 11, "white and grey")
    println(format(fish))
  }
}

/*
  Let’s make our printing library easier to use by defining some extension methods
  to provide better syntax:
      1. Create an object called PrintableSyntax
      2. Inside PrintableSyntax define an implicit class PrintableOps[A] to wrap up a value of type A
      3. In PrintableOps define the following methods:
          • format accepts an implicit Printable[A] and returns a String representation of the wrapped A;
          • print accepts an implicit Printable[A] and returns Unit.
            It prints the wrapped A to the console.
      4. Use the extension methods to print the example Cat you created in the previous exercise.
 */
object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String =
      Printable.format(value)

    def print(implicit printable: Printable[A]): Unit = Printable.print(value)
  }
}
