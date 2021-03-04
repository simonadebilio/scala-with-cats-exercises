package chapter04

object PostOrderCalculator {
  /*
  Start by writing a function evalOne that parses a single symbol into an instance of State.
  Use the code below as a template. Don’t worry about error handling for now — if the stack
  is in the wrong configuration, it’s OK to throw an exception.
   */
  import cats.data.State
  type CalcState[A] = State[List[Int], A]

  def operator(function: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case op2 :: op1 :: ops =>
        val result = function(op1, op2)
        (result :: ops, result)

      case _ => sys.error("Invalid operator")
    }

  def operand(op: Int): CalcState[Int] = State[List[Int], Int] { ops =>
    (op :: ops, op)
  }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  /*
  If this seems difficult, think about the basic form of the State instances you’re returning.
  Each instance represents a functional transformation from a stack to a pair of a stack and a result.
  You can ignore any wider context and focus on just that one step:

  State[List[Int], Int] { oldStack =>
    val newStack = someTransformation(oldStack)
    val result = someCalculation
    (newStack, result)
  }
 */

}
