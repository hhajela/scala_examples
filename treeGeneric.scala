sealed trait Tree[A] {

  def fold[B](leaffn: A => B)(nodefn: (B, B) => B): B = {

    this match {

      case Node(left, right) => nodefn(left.fold(leaffn)(nodefn), right.fold(leaffn)(nodefn))

      case Leaf(value) => leaffn(value)
    }
  }

  def getStr: String = fold(_.toString){ _ + " " + _ }
}

final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]
