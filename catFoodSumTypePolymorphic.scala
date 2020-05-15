sealed trait Feline {
  def dinner: FelineFood
}

final case object Lion extends Feline {
  def dinner: FelineFood = Antelope
}

final case object Tiger extends Feline {
  def dinner: FelineFood = TigerFood
}

final case object Panther extends Feline {
  def dinner: FelineFood = Licorice
}

final case class Cat(favouriteFood: String) extends Feline {
  def dinner: FelineFood = CatFood(favoriteFood)
}

sealed trait FelineFood

final case object Antelope extends FelineFood
final case object TigerFood extends FelineFood
final case object Licorice extends FelineFood
final case class CatFood(food: String) extends FelineFood




// implement a method dinner that returns the favorite food of a cat using
//
// sum type polymorphism
// add dinner defn to Feline and have all subtypes override

