import scala.annotation.tailrec

sealed trait IntList {
  def print: String = {
    this match {
      case Pair(head, tail) => s"$head->" + tail.print
      case End => "X"
    }
  }

  def fold[A](end: A, f: (Int, A) => A): A = {
    this match {
      case Pair(head, tail) => f(head, tail.fold(end, f))

      case End => end
    }
  }

  def length: Int = fold[Int](0, (x, y) => 1+y)

  def sum: Int = fold[Int](0, _ + _)

  def product: Int = fold[Int](1, _ * _)

  def double: IntList = fold[IntList](End, (h, t) => Pair(h*2, t))

}

case object End extends IntList {
  override def product: Int = 0
}

final case class Pair(head: Int, tail: IntList) extends IntList
