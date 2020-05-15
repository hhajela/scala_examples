sealed trait Json {
  def getStr: String
}

final case class IntValue(value: Int) extends Json {
  def getStr: String = s"$value"
}

final case class JsNumber(value: Double) extends Json {
  def getStr: String = s"$value"
}

final case class JsBoolean(value: Boolean) extends Json {
  def getStr: String = s"$value"
}

final case class JsString(value: String) extends Json {
  def getStr: String = "\"" + value + "\""
}

final case object JsNull extends Json {
  val getStr: String = "null"
}


sealed trait JsObject extends Json {

  def getStr: String = {

    "{\n" + getStrInternal
  }

  def getStrInternal: String = {
    this match {
      
      case ObjectCell(key,value,tail) => "\"" + key + "\" : " + value.getStr + ",\n" + tail.getStrInternal

      case ObjectEnd => "}"
    }
  }
}

final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject

case object ObjectEnd extends JsObject


sealed trait JsSequence extends Json {

  def getStr: String = {
    "[ " + getStrInternal
  }

  def getStrInternal: String = {
    this match {

      case SeqCell(value, tail) => value.getStr + ", " + tail.getStrInternal

      case SeqEnd => " ]"
    }
  }
}

final case class SeqCell(value: Json, tail: JsSequence) extends JsSequence

case object SeqEnd extends JsSequence

