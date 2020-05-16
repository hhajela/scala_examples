sealed trait Maybe[T] {

  def fold[B](empty: B)(full: T => B): B = {
    this match {

      case Full(value) => full(value)

      case Empty() => empty
    }
  }

  def flatMap[B](f: T => Maybe[B]): Maybe[B] = {
    this match {

      case Full(value) => f(value)

      case Empty() => Empty[B]()
    }
  }

  def map[B](f: T => B): Maybe[B] = flatMap( x => Full(f(x)) )

}

final case class Full[T](value: T) extends Maybe[T]

final case class Empty[T]() extends Maybe[T]


sealed trait Sum[A, B] {

  def fold[C](leftfn: A => C)(rightfn: B => C) = {
    this match {

      case Left(value) => leftfn(value)

      case Right(value) => rightfn(value)
    }
  }
}

final case class Left[A, B](value: A) extends Sum[A, B]

final case class Right[A, B](value: B) extends Sum[A, B]
