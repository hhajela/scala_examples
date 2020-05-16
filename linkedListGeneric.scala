sealed trait LinkedList[A] {

  def length: Int = fold[Int](0){ (x,y) => 1+y }

  def contains(value: A): Boolean = fold[Boolean](false){ (h, t) => (h == value) || t }

  def apply(index: Int): Result[A] = {
    this match {

      case Pair(head, tail) => if (index == 0) Success(head) else tail(index-1)

      case End() => Failure("index out of bounds")
    }
  }

  def fold[B](end: B)(f: (A, B) => B): B = {
    this match {

      case Pair(head, tail) => f(head, tail.fold(end)(f))

      case End() => end
    }
  }

  def map[B](f: A => B): LinkedList[B] = {
    this match {

      case Pair(head, tail) => Pair(f(head), tail.map(f))

      case End() => End()
    }
  }
}

final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

final case class End[A]() extends LinkedList[A]


sealed trait Result[A]

final case class Success[A](value: A) extends Result[A]

final case class Failure[A](errorMsg: String) extends Result[A]
