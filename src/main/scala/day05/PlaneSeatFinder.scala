package day05

import scala.io.{BufferedSource, Source}

object PlaneSeatFinder {

  case class Seat(row: Int, col:Int) {
    def seatID(): Int = row * 8 + col

    override def toString: String = s"Row = $row, Col = $col, SeatID = ${seatID()}"
  }

  def main(args: Array[String]): Unit = {

    findMaxSeatNumber("day05-input01.txt")


    val seats: List[Seat] = findMaxSeatNumber("day05-input02.txt")
    val missingSeats = seats.map(_.seatID()).sorted.sliding(2).flatMap{
      case List(a, b) if b-a > 1 => Some(b-1)
      case _ => None
    }.toList

    println(s"Missing Seat Number = ${missingSeats.headOption.getOrElse(0)}")
  }


  private def findMaxSeatNumber(fileName: String): List[Seat] = {
    val source: BufferedSource = Source.fromResource(fileName)
    val lines: List[String] = source.getLines().toList
    val result: List[Seat] = lines.map(line => find(line.toList, 0, 127, 0, 7))
    println(s"Total Seats = ${result.size}, Max Seat ID = ${result.map(_.seatID()).max}")
    source.close()
    result
  }

  def find(line: List[Char], lowerRowLimit:Int, highRowLimit: Int, lowColLimit: Int, highColLimit: Int) :Seat= {

    val rowMid = (highRowLimit - lowerRowLimit)/2 + lowerRowLimit
    val colMid = (highColLimit - lowColLimit)/2 + lowColLimit
    line match {
      case x :: tail => x match {
        case 'F' => find(tail, lowerRowLimit, rowMid, lowColLimit, highColLimit)
        case 'B' => find(tail, rowMid+1, highRowLimit, lowColLimit, highColLimit)
        case 'L' => find(tail, lowerRowLimit, highRowLimit, lowColLimit, colMid)
        case 'R' => find(tail, lowerRowLimit, highRowLimit, colMid+1, highColLimit)
      }
      case _ =>Seat(lowerRowLimit, lowColLimit)
    }
  }
}
