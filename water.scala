case class BottledWater(size: Int, source: WaterSource, carbonated: Boolean)

sealed trait WaterSource

case object Well extends WaterSource
case object Spring extends WaterSource
case object Tap extends WaterSource
