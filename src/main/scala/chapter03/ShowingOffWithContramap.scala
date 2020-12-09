package chapter03

object ShowingOffWithContramap {

  /*
  Implement the contramap method for Printable above.
  Start with the following code template and replace the ??? with a working method body.

  If you get stuck, think about the types. You need to turn value, which is of type B, into a String.
  What functions and methods do you have available and in what order do they need to be combined?
   */
  trait Printable[A] {
    self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        s"'${value}'"
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  /*
  Now define an instance of Printable for the following Box case class.
  You’ll need to write this as an implicit def as described in Section 1.2.3
   */
  final case class Box[A](value: A)

  /*
  Rather than writing out the complete definition from scratch (new Printable[Box] etc...),
  create your instance from an existing instance using contramap.
   */
  implicit def boxPrintable[A](
      implicit printableA: Printable[A]
  ): Printable[Box[A]] =
    printableA.contramap(_.value) // ((b: Box[A]) => b.value)

  def main(args: Array[String]): Unit = {
    assert(format("hello") == "'hello'")
    assert(format(true) == "yes")

    assert(format(Box("hello world")) == "'hello world'")
    assert(format(Box(true)) == "yes")
  }
}
