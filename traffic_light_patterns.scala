sealed trait TrafficLight {
  def next: TrafficLight = {
    this match {
      case Red => Green
      case Yellow => Red
      case Green => Yellow
    }
  }
}

case object Red extends TrafficLight

case object Yellow extends TrafficLight

case object Green extends TrafficLight

//external type


def next(t: TrafficLight): TrafficLight = {
  t match {
    case Red => Green
    case Yellow => Red
    case Green => Yellow
  }
}



