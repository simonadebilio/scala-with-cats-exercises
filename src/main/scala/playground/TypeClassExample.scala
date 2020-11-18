package playground

object TypeClassExample {
  case class Fish(n: String)
  case class Meat(n: String)

  // type class
  trait Seller[A] {
    def sell(obj: A): String
  }

  // type class instances
  object Seller {
    implicit val mario: Seller[Fish] = new Seller[Fish] {
      override def sell(obj: Fish): String = s"Sold fish: ${obj.n}"
    }

    implicit val giuseppina: Seller[Meat] = new Seller[Meat] {
      override def sell(obj: Meat): String = s"Sold meat: ${obj.n}"
    }
  }

  object Shop {
    def sell[A](obj: A)(implicit seller: Seller[A]): String =
      seller.sell(obj)

    def main(args: Array[String]): Unit = {
      println(sell(Fish("Cod")))
      println(sell(Meat("Sirloin")))
    }
  }
}
