import scala.annotation.tailrec

sealed trait IntList {
  def print: String = {
    this match {
      case Pair(head, tail) => s"$head->" + tail.print
      case End => "X"
    }
  }

  //tail rec optimized
  @tailrec
  def sum(total: Int = 0): Int = {
    this match {
      case End => total
      case Pair(head, tail) => tail.sum(head+total)
    }
  }

  def sum: Int = sum()

  @tailrec
  def length(curLength: Int): Int = {
    this match {
      case End => curLength
      case Pair(head, tail) => tail.length(1+curLength)
    }
  }

  @tailrec
  def product(totProd: Int): Int = {
    this match {
      case End => totProd
      case Pair(head, tail) => tail.product(totProd*head)
    }
  }

  @tailrec
  def double(curList: IntList = End): IntList = {
    this match {
      case End => curList
      case Pair(head, tail) => curList match {
        case End => tail.double(Pair(head*2, End))
        case Pair(h,t) => {
          ??? // operation which doubles head and attaches it to the end of curList
          tail.double(
        }
      }
    }
  }

  /* tail rec unoptimized
  @tailrec
  def sum: Int = {
    this match {
      case End => 0
      case Pair(head, tail) => head + tail.sum
    }
  }*/
}

case object End extends IntList

final case class Pair(head: Int, tail: IntList) extends IntList
