package day12

import scala.io.Source
/*
https://adventofcode.com/2020/day/12
25
1589
286
23960
 */
object ShipNavigation {

  case class Inst(direction: String, unit: Int)
  case class Position(eastWest:Int, northSouth: Int)

  val clockWiseDirections: Array[String] = Array("E", "S", "W", "N")

  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day12-input01.txt")
    val input2 = parseInput("day12-input02.txt")

    navigateUsingDirection(Position(0,0),"E", input1)
    navigateUsingDirection(Position(0,0),"E", input2)

    navigateUsingWayPoints(Position(0,0), Position(10, 1), input1)
    navigateUsingWayPoints(Position(0,0), Position(10, 1), input2)
  }


  def parseInput(fileName: String) = {
    val regex = "([NSEWLRF]{1})([0-9]+)".r
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().map(_ match {
      case regex(dir, unit) => Inst(dir, unit.toInt)
    }).toList
    source.close()
    lines
  }


  def navigateUsingDirection(startingPos: Position, startingDirection: String, input: List[Inst]) = {
    var currentPos = startingPos
    var currentFacing = startingDirection
    input.foreach(i => {
      i.direction match {
        case "F" => currentPos = moveShipOrWayPoint(currentPos, Inst(currentFacing, i.unit))
        case "R" | "L" => currentFacing = changeDirection(currentFacing, i)
        case _ => currentPos = moveShipOrWayPoint(currentPos, i)
      }
    })
    println(Math.abs(currentPos.eastWest) + Math.abs(currentPos.northSouth))
  }

  def navigateUsingWayPoints(startingPos: Position, wayPoint: Position, input: List[Inst]) = {

    var currentPos = startingPos
    var currentWayPoint = wayPoint

    input.foreach(i => i.direction match {
        case "F" => currentPos = moveShipForwardInWayPointDirection(currentPos, currentWayPoint, i)
        case "N"|"S"|"E"|"W" => currentWayPoint = moveShipOrWayPoint(currentWayPoint, i)
        case _ => currentWayPoint = changeWayPoint(currentWayPoint, i)
      }
    )
    println(Math.abs(currentPos.eastWest) + Math.abs(currentPos.northSouth))
  }




  private def moveShipOrWayPoint(currentPos: Position, i: Inst) = {
    i.direction match {
      case "N" => currentPos.copy(northSouth = currentPos.northSouth + i.unit)
      case "S" => currentPos.copy(northSouth = currentPos.northSouth - i.unit)
      case "E" => currentPos.copy(eastWest = currentPos.eastWest + i.unit)
      case "W" => currentPos.copy(eastWest = currentPos.eastWest - i.unit)
    }
  }

  private def changeDirection(currentFacing: String, i: Inst) = {
    val indexOfCurrentDirection = clockWiseDirections.indexOf(currentFacing)
    val degrees = i.direction match {
      case "L" => -i.unit
      case _ => i.unit
    }
    val indexOfNewDirection = (degrees/90 + clockWiseDirections.size + indexOfCurrentDirection) % clockWiseDirections.size
    clockWiseDirections(indexOfNewDirection)
  }

  private def moveShipForwardInWayPointDirection(currentPos: Position, currentWayPoint: Position, i: Inst) = {
    currentPos.copy(
      eastWest = currentPos.eastWest + currentWayPoint.eastWest * i.unit,
      northSouth = currentPos.northSouth + currentWayPoint.northSouth * i.unit)
  }


  private def changeWayPoint(currentWayPoint: Position, i: Inst) = {
    val currentEastWest = if (currentWayPoint.eastWest >= 0) "E" else "W"
    val currentNorthSouth = if (currentWayPoint.northSouth >= 0) "N" else "S"
    val dir1 = changeDirection(currentEastWest, i)
    val dir2 = changeDirection(currentNorthSouth, i)
    val newWayPoint = (dir1, dir2) match {
      case ("E", "N") => currentWayPoint.copy(eastWest = Math.abs(currentWayPoint.eastWest), northSouth = Math.abs(currentWayPoint.northSouth))
      case ("E", "S") => currentWayPoint.copy(eastWest = Math.abs(currentWayPoint.eastWest), northSouth = -Math.abs(currentWayPoint.northSouth))
      case ("W", "N") => currentWayPoint.copy(eastWest = -Math.abs(currentWayPoint.eastWest), northSouth = Math.abs(currentWayPoint.northSouth))
      case ("W", "S") => currentWayPoint.copy(eastWest = -Math.abs(currentWayPoint.eastWest), northSouth = -Math.abs(currentWayPoint.northSouth))
      case ("N", "E") => currentWayPoint.copy(northSouth = Math.abs(currentWayPoint.eastWest), eastWest = Math.abs(currentWayPoint.northSouth))
      case ("N", "W") => currentWayPoint.copy(northSouth = Math.abs(currentWayPoint.eastWest), eastWest = -Math.abs(currentWayPoint.northSouth))
      case ("S", "E") => currentWayPoint.copy(northSouth = -Math.abs(currentWayPoint.eastWest), eastWest = Math.abs(currentWayPoint.northSouth))
      case ("S", "W") => currentWayPoint.copy(northSouth = -Math.abs(currentWayPoint.eastWest), eastWest = -Math.abs(currentWayPoint.northSouth))
    }
    newWayPoint
  }
}
