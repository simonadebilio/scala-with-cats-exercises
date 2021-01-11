package chapter04

object GettingFuncy {

  /*
  Every monad is also a functor. We can define map in the same way for every
  monad using the existing methods, flatMap and pure:
  Try defining map yourself now.
   */
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(afb: A => F[B]): F[B]
    def map[A, B](fa: F[A])(ab: A => B): F[B] =
      flatMap(fa)((a: A) => pure(ab(a)))
    // (a: A) => ab(a)
    // def ab(a: A): B = ???
    // def ab: A => B = (a: A) => ???

  }

}
