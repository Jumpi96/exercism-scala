
object Bearing extends Enumeration {
  type Bearing = Value
  val North, South, West, East = Value
}

class Robot private (val bearing: Bearing.Value, val coordinates: (Int, Int)) {

  def simulate(orders: String): Robot = {
    orders.foldLeft(this)((robot, order) => order match {
      case 'A' => robot.advance
      case 'R' => robot.turnRight
      case 'L' => robot.turnLeft
    })
  }

  def turnRight: Robot = {
    bearing match {
      case Bearing.North => Robot(Bearing.East, coordinates)
      case Bearing.South => Robot(Bearing.West, coordinates)
      case Bearing.West => Robot(Bearing.North, coordinates)
      case Bearing.East => Robot(Bearing.South, coordinates)
    }
  }

  def turnLeft: Robot = {
    bearing match {
      case Bearing.North => Robot(Bearing.West, coordinates)
      case Bearing.South => Robot(Bearing.East, coordinates)
      case Bearing.West => Robot(Bearing.South, coordinates)
      case Bearing.East => Robot(Bearing.North, coordinates)
    }
  }

  def advance: Robot = {
    bearing match {
      case Bearing.North => Robot(bearing, (coordinates._1, coordinates._2 + 1))
      case Bearing.South => Robot(bearing, (coordinates._1, coordinates._2 - 1))
      case Bearing.West => Robot(bearing, (coordinates._1 - 1, coordinates._2))
      case Bearing.East => Robot(bearing, (coordinates._1 + 1, coordinates._2))
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: Robot => that.bearing == bearing && that.coordinates == coordinates
    case _ => false
  }
  
}

object Robot {
  def apply(bearing: Bearing.Value, coordinates: (Int, Int)) = new Robot(bearing, coordinates)
}