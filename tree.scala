sealed trait Tree {

  def sum: Int = {
    this match {
      case Node(left,right) => left.sum + right.sum
      case Leaf(value) => value
    }
  }

  def double: Tree = {
    this match {
      case Leaf(value) => Leaf(2*value)
      case Node(left, right) => Node(left.double, right.double)
    }
  }

  def printTree(offset: Int = 0): String = {
    this match {
      case Leaf(value) => s"$value\n"
      case Node(left, right) => {
        val firstPart = ".\n" + (" "*offset) + "|\n"
        val secondPart = (" "*offset) + "--->" + left.printTree(offset+4)
        val lastPart = (" "*offset) + "--->" + right.printTree(offset+4) + "\n"
        
        firstPart + secondPart + lastPart
      }
    }
  }
}

final case class Node(left: Tree, right: Tree) extends Tree

final case class Leaf(value: Int) extends Tree




