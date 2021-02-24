package chapter04

import cats.data.Reader
import cats.implicits.catsSyntaxApplicativeId

object HackingOnReaders {
  /*
  The classic use of Readers is to build programs that accept a configuration
  as a parameter. Let’s ground this with a complete example of a simple login
  system. Our configuration will consist of two databases: a list of valid users
  and a list of their passwords:
   */
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  /*
  Start by creating a type alias DbReader for a Reader that consumes a Db as
  input. This will make the rest of our code shorter.
   */
  type DbReader[A] = Reader[Db, A]

  /*
  Now create methods that generate DbReaders to look up the username for
  an Int user ID, and look up the password for a String username. The type
  signatures should be as follows:
   */
  def findUsername(userId: Int): DbReader[Option[String]] =
    new DbReader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    new DbReader(db =>
      db.passwords.get(username) match {
        case None    => false
        case Some(p) => p == password
      }
    )

  /*
  Finally create a checkLogin method to check the password for a given user ID.
  The type signature should be as follows:
   */
  def myCheckLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap {
      case None         => false.pure[DbReader]
      case Some(userId) => checkPassword(userId, password)
    }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username
        .map { username => checkPassword(username, password) }
        .getOrElse {
          false.pure[DbReader]
        }
    } yield passwordOk
}
