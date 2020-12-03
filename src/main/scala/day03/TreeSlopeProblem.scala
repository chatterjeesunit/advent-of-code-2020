package day03

import scala.io.Source


/**
 * https://adventofcode.com/2020/day/3
 *
 * day03-input01.txt - Expected Result = (2, 7, 3, 4, 2) and multiplication result = 336
 * day03-input02.txt - Expected Result = (53, 282, 54, 54, 22) and multiplication result = 958815792
 */


object TreeSlopeProblem {

  case class SlopeIncrement(x: Int, y: Int)

  case class FinalResult(totalTrees: List[Int]) {
    def multiplicationResult: Int = totalTrees.reduce((x, y)=> x* y)
    override def toString: String = s"Total Trees = $totalTrees, result = $multiplicationResult"
  }


  def main(args: Array[String]): Unit = {

    val slopeIncrements: List[SlopeIncrement] = List(
      SlopeIncrement(1,1), SlopeIncrement(3,1), SlopeIncrement(5,1), SlopeIncrement(7,1), SlopeIncrement(1,2))

    val lines1: List[String] = Source.fromResource("day03-input01.txt").getLines().toList
    val result1: FinalResult = calculateMultipleSlopesResult(lines1, slopeIncrements)
    println(s"Result Input 01 : $result1")

    val lines2: List[String] = Source.fromResource("day03-input02.txt").getLines().toList
    val result2: FinalResult = calculateMultipleSlopesResult(lines2, slopeIncrements)
    println(s"Result Input 02 : $result2")
  }





  def calculateMultipleSlopesResult(lines: List[String], slopes: List[SlopeIncrement]): FinalResult = {

    def calculateTreesEncountered(lines: List[String], increment: SlopeIncrement, currentXPos:Int, currentYPos:Int,
                                  currentRepeatCounter:Int, totalTrees:Int ): Int = {
      if(currentYPos >= lines.size)
        totalTrees
      else {
        val (line, newRepeatCounter) = expandLine(lines(currentYPos), currentXPos, currentRepeatCounter)
        val newTreeCount = if (line(currentXPos).equals('#')) totalTrees + 1 else totalTrees
        calculateTreesEncountered(lines, increment, currentXPos+increment.x, currentYPos+increment.y, newRepeatCounter, newTreeCount)
      }
    }

    def expandLine(originalLine: String, pos: Int, currentRepeatCounter:Int): (String, Int) = {
      if(originalLine.length > pos)
        (originalLine, currentRepeatCounter)
      else {
        val newRepeatCounter: Int = if(originalLine.length * currentRepeatCounter > pos) currentRepeatCounter else currentRepeatCounter+1
        val result:String = (
          for (
            n <- 1 to newRepeatCounter
          ) yield originalLine
          ).reduce((x,y) => x.concat(y))
        (result, newRepeatCounter)
      }
    }


    val trees: List[Int] = slopes.map(slope => {
      calculateTreesEncountered(lines, slope, slope.x, slope.y, 1, 0)
    })

    FinalResult(trees)
  }
}
