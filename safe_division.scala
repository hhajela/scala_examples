sealed trait DivisionResult

case object Infinite extends DivisionResult

case class Finite(quotient: Int, remainder: Int) extends DivisionResult

object divide {
  def apply(dividend: Int, divisor: Int): DivisionResult = {
    if (divisor==0)
      Infinite
    else
      Finite(dividend/divisor, dividend%divisor)
  }
}


def doSome = {
  println("multiply 3 by 5")
  val v = 3*5
  println("divide by some other number")
  val z = divide(v,2)

  // error check on z
  val finalResult: String = {
    z match {
      case Infinite => "Division Failed, possible divide by zero"
      case Finite(q,r) => s"Division result $q, remainder $r"
    }
  }
  println(finalResult)
}

