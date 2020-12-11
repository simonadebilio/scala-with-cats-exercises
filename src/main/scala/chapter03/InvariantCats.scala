package chapter03

import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._ // for |+|

object InvariantCats {
  /* Imagine we want to produce a Monoid for Scala's Symbol type.
  Cats doesn't provide a Monoid for Symbol but it does provide a Monoid for a similar type: String.
  We can write our new semigroup with an empty method that relies on the empty String,
  and a combine method that works as follows:
    1. accept two Symbols as parameters;
    2. convert the Symbols to Strings;
    3. combine the Strings using Monoid[String];
    4. convert the result back to a Symbol.
   */
  implicit val symbolMonoid: Monoid[Symbol] = new Monoid[Symbol] {
    override def empty: Symbol = Symbol("")

    override def combine(x: Symbol, y: Symbol): Symbol = {
      Symbol(x.name |+| y.name)
    }
  }

//  implicit def symbolMonoid2: Monoid[Symbol] =
//    Monoid[String].imap((s: String) => Symbol(s))((sym: Symbol) => sym.name)

  def main(args: Array[String]): Unit = {
    Monoid[Symbol].empty
    Symbol("a") |+| Symbol("few") |+| Symbol("words")
  }
}
