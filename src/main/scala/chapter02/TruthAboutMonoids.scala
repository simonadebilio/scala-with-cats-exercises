package chapter02

import chapter02.Monoid.identityLaw
import chapter02.Semigroup.associativeLaw

/*
We’ve seen a few examples of monoids but there are plenty more to be found.
Consider Boolean. How many monoids can you define for this type?
For each monoid, define the combine and empty operations and convince yourself that
the monoid laws hold. Use the following definitions as a starting point:
*/

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Semigroup[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
    monoid

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }
}

object BooleanAndMonoid {
  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  def main(args: Array[String]): Unit = Main.verifyLaws
}

object BooleanOrMonoid {
  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  def main(args: Array[String]): Unit = Main.verifyLaws
}

object BooleanXorMonoid {
  implicit val booleanXorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  def main(args: Array[String]): Unit = Main.verifyLaws
}

object BooleanXnorMonoid {
  implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)

    //      (x && y) || (!x && !y)
  }

  def main(args: Array[String]): Unit = Main.verifyLaws
}

object Main {
  def verifyLaws(implicit monoid: Monoid[Boolean]): Unit = {
    assert(identityLaw(true))
    assert(identityLaw(false))

    val values: Seq[Boolean] = Seq(true, false)
    val combinations: Seq[(Boolean, Boolean, Boolean)] = for {
      a <- values
      b <- values
      c <- values
    } yield (a, b, c)

    combinations.foreach(c => assert(associativeLaw[Boolean] _ tupled c))
  }
}
