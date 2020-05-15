sealed trait Calculation

final case class Failure(error: String) extends Calculation
final case class Success(result: Int) extends Calculation

object Calculator {

  def +(calculation: Calculation, b: Int): Calculation = {
    calculation match {
      case Failure(error) => calculation
      case Success(result) => Success(result+b)
    }
  }

  def +(a: Int, b: Int): Calculation = Success(a+b)
  
  def -(calculation: Calculation, b: Int): Calculation = {
    calculation match {
      case Failure(error) => calculation
      case Success(result) => Success(result-b)
    }
  }

  def -(a: Int, b: Int): Calculation = Success(a-b)

  def /(calculation: Calculation, b: Int): Calculation = {
    calculation match {
      case Failure(error) => Failure(error)
      case Success(result) => /(result,b)
    }
  }

  def /(a: Int, b: Int): Calculation = if (b == 0) Failure("divide by zero error") else Success(a/b)
}

