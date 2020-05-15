sealed trait CalcResult

case class Failure(error: String) extends CalcResult

case class Success(result: Double) extends CalcResult


object Calc {

  def multiply(a: Double, b: Double): CalcResult = Success(a*b)

  def add(a: Double, b: Double): CalcResult = Success(a+b)

  def divide(a: Double, b: Double): CalcResult = if (b==0) Failure("Divide by zero error") else Success(a/b)
  
  def subtract(a: Double, b: Double): CalcResult = Success(a-b)
}
