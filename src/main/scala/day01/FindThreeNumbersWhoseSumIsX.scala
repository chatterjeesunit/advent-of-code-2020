package day01

import scala.io.Source

/**
 * https://adventofcode.com/2020/day/1
 * Puzzle 2
 */
object FindThreeNumbersWhoseSumIsX {

  case class Solution(num1:Long, num2:Long, num3:Long) {
    def result():Long = num1 * num2 * num3

    override def toString: String = s"Num1 = $num1, Num2 = $num2, Num3 = $num3, Result = $result"
  }

  def main(args: Array[String]): Unit = {

    val desiredSum = 2020l
    val lines: List[Long] = Source.fromResource("day01-input.txt").getLines().map(_.toLong).toList.sorted
    val result: Option[Solution] = solveUsingSortAndLoop(desiredSum, lines)
    println(result.map(_.toString).getOrElse("No Result"))
  }

  private def solveUsingSortAndLoop(desiredSum: Long, numbers: List[Long]): Option[Solution] = {
    if(numbers.size < 3) return Option.empty



    def findNumbers(): Option[Solution] = {
      var firstNum = numbers(0)
      var i = 1
      var j = numbers.size-1
      while (i <= numbers.size - 1 && j >= 0) {
        val currentSum = numbers(i) + numbers(j)
        if (currentSum == desiredSum - firstNum)
          return Some(Solution(firstNum, numbers(i), numbers(j)))
        else if (currentSum < desiredSum)
          i = i + 1
        else
          j = j - 1
      }
      None
    }

    findNumbers match {
      case Some(solution) => return Some(solution)
      case None => solveUsingSortAndLoop(desiredSum, numbers.takeRight(1))
    }
  }
}


