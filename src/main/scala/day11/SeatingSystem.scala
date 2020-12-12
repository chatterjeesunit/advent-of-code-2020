package day11

import scala.collection.mutable.ListBuffer
import scala.io.Source
/*
https://adventofcode.com/2020/day/11
34, 37
5146, 2418
45,26
5420, 2144
 */
object SeatingSystem {


  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day11-input01.txt")
    val input2 = parseInput("day11-input02.txt")



    val (_, emptySeats1, occupiedSeats1) = occupySeats(input1, false, 4)
    println(s"Total Empty Seats = ${emptySeats1}, Total Occupied Seats = ${occupiedSeats1}")

    val (_, emptySeats2, occupiedSeats2) = occupySeats(input2, false, 4)
    println(s"Total Empty Seats = ${emptySeats2}, Total Occupied Seats = ${occupiedSeats2}")

    val (_, emptySeats3, occupiedSeats3) = occupySeats(input1, true, 5)
    println(s"Total Empty Seats = ${emptySeats3}, Total Occupied Seats = ${occupiedSeats3}")

    val (_, emptySeats4, occupiedSeats4) = occupySeats(input2, true, 5)
    println(s"Total Empty Seats = ${emptySeats4}, Total Occupied Seats = ${occupiedSeats4}")

  }


  def parseInput(fileName: String): ListBuffer[ListBuffer[String]] = {
    val source: Source = Source.fromResource(fileName)
    val lines: ListBuffer[String] = source.getLines().to(ListBuffer)
    source.close()
    val result: ListBuffer[ListBuffer[String]] = lines.map(_.toList.map(_.toString).to(ListBuffer))
    result
  }

  def occupySeats(input: ListBuffer[ListBuffer[String]], checkRecursively: Boolean, maxOccupySeatsToCheck: Int): (ListBuffer[ListBuffer[String]], Int, Int) = {
//    printlnSeats(input)
    val newInput = cloneList(input)
    val maxIPos = input.size - 1
    val maxJPos = input(0).size - 1
    var seatsChanged = false
    for(i <- 0 to maxIPos) {
      for(j <- 0 to maxJPos) {
        val choice = seatChoice(input, i, j, checkRecursively, maxOccupySeatsToCheck)
        choice match {
          case "EMPTY" => {
            newInput(i)(j) = "L"
            seatsChanged = true
          }
          case "OCCUPY" => {
            newInput(i)(j) = "#"
            seatsChanged = true
          }
          case _ => //Do Nothing
        }
      }
    }
    val emptySeats = countEmptySeats(newInput)
    val occupiedSeats = countOccupiedSeats(newInput)
//    println(s"Seats Changed = $seatsChanged, Total Empty Seats = ${emptySeats}, , Total Occupied Seats = ${occupiedSeats}")
    if(seatsChanged) {
      occupySeats(newInput, checkRecursively, maxOccupySeatsToCheck)
    } else {
      (newInput, emptySeats, occupiedSeats)
    }
  }

  def countEmptySeats(input: ListBuffer[ListBuffer[String]]): Int = {
    input.map(l => l.filter(seat => isSeatEmpty(seat)).size).sum
  }

  def countOccupiedSeats(input: ListBuffer[ListBuffer[String]]): Int = {
    input.map(l => l.filter(seat => isSeatOccupied(seat)).size).sum
  }

  def isFloor(seat: String): Boolean = seat.equals(".")
  def isSeatEmpty(seat: String): Boolean = seat.equals("L")
  def isSeatOccupied(seat: String): Boolean = "#".equals(seat)


  private def isValidSeatPosition(input: ListBuffer[ListBuffer[String]], i: Int, j: Int) = {
    val maxIPos = input.size
    val maxJPos = input(0).size
    i >= 0 && i < maxIPos && j >= 0 && j < maxJPos
  }

  def seatChoice(input: ListBuffer[ListBuffer[String]], i: Int, j: Int, checkRecursively: Boolean, maxOccupySeatsToCheck: Int): String = {

    if(isFloor(input(i)(j))) "DO NOTHING"
    else {
      val totalOccupiedSeatsRecursively =
        checkAdjacentSeats(input, i, j, -1, 0, checkRecursively) +
          checkAdjacentSeats(input, i, j, -1, -1, checkRecursively) +
          checkAdjacentSeats(input, i, j, 0, -1, checkRecursively) +
          checkAdjacentSeats(input, i, j, 1, -1, checkRecursively) +
          checkAdjacentSeats(input, i, j, 1, 0, checkRecursively) +
          checkAdjacentSeats(input, i, j, 1, 1, checkRecursively) +
          checkAdjacentSeats(input, i, j, 0, 1, checkRecursively) +
          checkAdjacentSeats(input, i, j, -1, 1, checkRecursively)


      if(isSeatEmpty(input(i)(j)) && totalOccupiedSeatsRecursively == 0 ) "OCCUPY"
      else if(isSeatOccupied(input(i)(j)) && totalOccupiedSeatsRecursively >= maxOccupySeatsToCheck ) "EMPTY"
      else "DO NOTHING"
    }

  }

  def printlnSeats(input: ListBuffer[ListBuffer[String]]): Unit = {
    println(input.map(_.mkString(" ")).mkString("\n"))
  }

  def cloneList(input: ListBuffer[ListBuffer[String]]) : ListBuffer[ListBuffer[String]] = {
    val l = new ListBuffer[ListBuffer[String]]()
    val maxIIndex = input.size - 1
    val maxJIndex = input(0).size - 1
    for(i <- 0 to maxIIndex) {
      l += new ListBuffer[String]()
      for(j <- 0 to maxJIndex) {
        l(i) += input(i)(j)
      }
    }
    l
  }



  def checkAdjacentSeats(input: ListBuffer[ListBuffer[String]], i:Int, j:Int, iIncrement: Int, jIncrement: Int, checkRecursively: Boolean): Int = {
    val (newI, newJ) = (i+iIncrement, j+jIncrement)
    if(isValidSeatPosition(input, newI, newJ)){
      if(isSeatOccupied(input(newI)(newJ)))
        1
      else if (isSeatEmpty(input(newI)(newJ)))
        0
      else if(checkRecursively)
        checkAdjacentSeats(input, newI, newJ, iIncrement, jIncrement, checkRecursively)
      else
        0
    } else 0
  }

}
