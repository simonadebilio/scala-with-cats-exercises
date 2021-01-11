package chapter04

import cats.MonadError

/*
Implement a method validateAdult with the following signature

When passed an age greater than or equal to 18 it should return that
value as a success. Otherwise it should return an error represented as an
IllegalArgumentException
 */

object Abstracting {
  def validateAdult[F[_]](
      age: Int
  )(implicit me: MonadError[F, Throwable]): F[Int] = {
    if (age >= 18) me.pure(age)
    else
      me.raiseError(
        new IllegalArgumentException("Age must be greater than or equal to 18")
      )
  }
}
