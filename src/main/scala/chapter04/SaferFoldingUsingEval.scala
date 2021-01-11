package chapter04

import cats.Eval

/*
The naive implementation of foldRight below is not stack safe. Make it so using Eval:

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail => fn(head, foldRight(tail, acc)(fn))
      case Nil => acc
             }
 */
object SaferFoldingUsingEval {
  def foldRightEval[A, B](as: List[A], acc: Eval[B])(
      fn: (A, Eval[B]) => Eval[B]
  ): Eval[B] = {
    as match {
      case Nil          => acc
      case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    }
  }

  /*
  We can redefine foldRight simply in terms of foldRightEval
   */
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value
  }
}
