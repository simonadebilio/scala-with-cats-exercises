package chapter03

object CodecExercise {
  trait Codec[A] {
    self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  // Demonstrate your imap method works by creating a Codec for Double.
  def stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  //Finally, implement a Codec for the following Box type:
  final case class Box[A](value: A)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap((a: A) => Box(a), (b: Box[A]) => b.value) // c.imap(Box(_), _.value)

  def main(args: Array[String]): Unit = {
    println(encode(123.4))
    println(decode[Double]("123.4"))

    println(encode(Box(123.4)))
    println(decode[Box[Double]]("123.4"))
  }
}
