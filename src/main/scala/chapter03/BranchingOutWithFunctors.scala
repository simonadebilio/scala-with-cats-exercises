package chapter03

import cats.Functor
import cats.implicits.toFunctorOps

object BranchingOutWithFunctors {

  /*
  Write a Functor for the following binary tree data type.
  Verify that the code works as expected on instances of Branch and Leaf:
  */
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          case Leaf(value) => Leaf(f(value))
        }
    }
  }

  def main(args: Array[String]): Unit = {
    Tree.leaf(100).map(_ * 2)
  }
}
