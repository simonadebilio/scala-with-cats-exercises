package chapter02

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup

/*
The cutting edge SuperAdder v3.5a‐32 is the world’s first choice for adding
together numbers. The main function in the program has signature def
add(items: List[Int]): Int. In a tragic accident this code is deleted!
Rewrite the method and save the day!
*/
object SuperAdder {
  def add(items: List[Int]): Int = items.sum
}

/*
Well done! SuperAdder’s market share continues to grow, and now there is demand for additional functionality.
People now want to add List[Option[Int]]. Change add so this is possible.
The SuperAdder code base is of the highest quality, so make sure there is no code duplication!
 */
object SuperAdder2 {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)
}

/*
SuperAdder is entering the POS (point‐of‐sale, not the other POS) market.
Now we want to add up Orders: case class Order(totalCost: Double, quantity: Double)
We need to release this code really soon so we can’t make any modifications to add. Make it so!
 */
object SuperAdder3 {

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)

  def main(args: Array[String]): Unit = {
    assert(add(List(Order(1, 2), Order(2, 3))) == Order(3, 5))
  }
}
