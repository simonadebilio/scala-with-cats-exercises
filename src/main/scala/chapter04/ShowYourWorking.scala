package chapter04

import cats.data.{Reader, Writer}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object ShowYourWorking {
  /*
  The factorial function below computes a factorial and prints out the intermediate steps as it runs.
  The slowly helper function ensures this takes a while to run, even on the very small examples below:
    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)
    def factorial(n: Int): Int = {
      val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
     }

   Rewrite factorial so it captures the log messages in a Writer.
   Demonstrate that this allows us to reliably separate the logs for concurrent computations.
   */

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def writerFactorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(writerFactorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  final case class Cat(name: String, favoriteFood: String)
  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)
}
