sealed trait Feline

final case object Panther extends Feline
final case object Lion extends Feline
final case object Tiger extends Feline
final case class Cat(favFood: String) extends Feline

sealed trait FelineFood

final case object Antelope
final case object TigerFood
final case object Licorice
final case class CatFood(name: String)


object dinner {
  def apply(f: Feline): FelineFood = {
    f match {
      case Panther => Licorice
      case Tiger => TigerFood
      case Lion => Antelope
      case Cat(foodName) => CatFood(foodName)
    }
  }
}


//defining dinner method using sum type pattern matching
//
//another possibility is to define the pattern matching method in Feline
//like so

/*
sealed trait Feline {
  def dinner: FelineFood = {
    this match {
      case Panther => Licorice
      case Tiger => TigerFood
      case Lion => Antelope
      case Cat(favFood) => CatFood(favFood)
    }
  }
}*/

//disadvantage is it probably breaks the respecting levels of abstraction rule
//only applies if we are using a sealed trait
//otherwise better to go with abstract class based polymorphism
