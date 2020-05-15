sealed trait Shape {
  def sides: Int
  def area: Double
  def perimeter: Double
  def color: Color
}

sealed trait Rectangular extends Shape {
  val sides: Int = 4

  val length: Double
  val breadth: Double

  def area: Double = length*breadth

  def perimeter: Double = 2*length + 2*breadth
}


case class Square(side: Double, color: Color) extends Rectangular {
  val length: Double = side
  val breadth: Double = side
}

case class Rectangle(val length: Double, val breadth: Double, color: Color) extends Rectangular


case class Circle(rad: Double, color: Color) extends Shape {
  val sides: Int = 0

  def area: Double = math.Pi*rad*rad

  def perimeter: Double = 2*math.Pi*rad
}


sealed trait Color {
  def red: Int
  def blue: Int
  def green: Int

  // colors for which this value is in [128,255] are light, others ([0,127]) are dark
  def isLight: Boolean = math.sqrt( 0.299*red*red + 0.587*green*green + 0.114*blue*blue ) > 127
}


case object Pink extends Color {
  val red: Int = 255
  val blue: Int = 150
  val green: Int = 0
}

case object Red extends Color {
  val red: Int = 255
  val blue: Int = 0
  val green: Int = 0
}

case object Yellow extends Color {
  val red: Int = 255
  val blue: Int = 0
  val green: Int = 255
}

case class CustomColor(val red: Int, val green: Int, val blue: Int) extends Color


object Draw {
  def apply(s: Shape): String = {
    s match {
      case Circle(rad, color) => s"${getColor(color)} Circle of radius $rad units"
      case Rectangle(l, b, color) => s"${getColor(color)} Rectangle of length $l and breadth $b units"
      case Square(l,color) => s"${getColor(color)} Square of length $l"
    }
  }

  def getColor(c: Color): String = {
    c match {
      case Red => "red"
      case Pink => "pink"
      case Yellow => "yellow"
      case CustomColor(_,_,_) => if (c.isLight) "light" else "dark"
    }
  }
}


