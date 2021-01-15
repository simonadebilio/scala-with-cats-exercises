package chapter04

import cats.Id
import cats.syntax.either._

object MonadicSecretIdentities {

  /*
  Implement pure, map, and flatMap for Id! What interesting discoveries do you uncover about the implementation?
   */

  def pure[A](a: A): Id[A] = a
  def map[A, B](a: Id[A], f: A => B): Id[B] = f(a)
  def flatMap[A, B](a: Id[A], f: A => Id[B]): Id[B] = f(a)
}
