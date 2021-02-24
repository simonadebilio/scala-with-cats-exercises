package chapter02

import chapter02.Monoid.identityLaw
import chapter02.Semigroup.associativeLaw

//  What monoids and semigroups are there for sets?
object SetUnionMonoid {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  def main(args: Array[String]): Unit = Laws.verifyMonoidLaws
}

object SetIntersectionSemigroup {
  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
    }

  def main(args: Array[String]): Unit = Laws.verifyAssociativeLaw
}

object SymmetricDifferenceMonoid {
  implicit def symmetricDifferenceMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty

      override def combine(x: Set[A], y: Set[A]): Set[A] =
        (x union y) diff (x intersect y)
//      (x diff y) union (y diff x)
    }

  def main(args: Array[String]): Unit = Laws.verifyMonoidLaws
}

object Laws {
  def verifyAssociativeLaw(implicit semigroup: Semigroup[Set[Int]]): Unit = {
    val values: Seq[Set[Int]] = Seq(Set.empty, Set(1, 2, 3), Set(4, 5, 6))
    val combinations: Seq[(Set[Int], Set[Int], Set[Int])] = for {
      a <- values
      b <- values
      c <- values
    } yield (a, b, c)

    combinations.foreach(c => assert(associativeLaw[Set[Int]] _ tupled c))
  }

  def verifyMonoidLaws(implicit monoid: Monoid[Set[Int]]): Unit = {
    verifyAssociativeLaw

    assert(identityLaw(Set.empty[Int]))
    assert(identityLaw(Set(1)))
  }
}
