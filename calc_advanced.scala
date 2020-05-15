sealed trait Expression {
  def eval: Result
}

sealed trait Result

final case class Success(value: Double) extends Result

final case class Failure(errorMessage: String) extends Result



final case class Addition(left: Expression, right: Expression) extends Expression {

  def eval: Result = {
    val leftVal = left.eval
    val rightVal = right.eval

    leftVal match {

      case Success(value) => {

        rightVal match {
          
          case Success(value2) => Success(value + value2)

          case Failure(msg) => Failure(msg)
        }
      }

      case Failure(msg) => Failure(msg)

    }
  }
}

final case class Subtraction(left: Expression, right: Expression) extends Expression {

  def eval: Result = {
    val leftVal = left.eval
    val rightVal = right.eval

    leftVal match {

      case Success(value) => {
        rightVal match {
          case Success(value2) => Success(value - value2)

          case Failure(msg) => Failure(msg)
        }
      }

      case Failure(msg) => Failure(msg)
    }
  }
}

final case class Number(value: Double) extends Expression {
  def eval: Result = Success(value)
}

final case class Division(left: Expression, right: Expression) extends Expression {

  def eval: Result = {
    val leftVal = left.eval
    val rightVal = right.eval

    leftVal match {

      case Success(value) => {
        rightVal match {

          case Success(0) => Failure("Division operation failed: Divide by Zero error")

          case Success(value2) => Success(value / value2)

          case Failure(msg) => Failure(msg)
        }
      }

      case Failure(msg) => Failure(msg)
    }
  }
}

final case class SquareRoot(operand: Expression) extends Expression {

  def eval: Result = {
    val opVal = operand.eval

    opVal match {

      case Success(value) => if (value<0) Failure("Sqrt Operation Failed, operand is out of range (-ve) ") else Success(math.sqrt(value))

      case Failure(msg) => Failure(msg)
    }
  }

}




