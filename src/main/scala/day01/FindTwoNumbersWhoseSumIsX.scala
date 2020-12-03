package day01

import scala.io.Source

/**
 * https://adventofcode.com/2020/day/1
 * Puzzle 1
 */
object FindTwoNumbersWhoseSumIsX {

  case class Solution(num1:Int, num2:Int) {
    def result():Int = num1 * num2

    override def toString: String = s"Num1 = $num1, Num2 = $num2, Result = $result"
  }

  def main(args: Array[String]): Unit = {

    val desiredSum = 2020
    val solution1 = solveUsingHashSet(desiredSum, "day01-input.txt").map(_.toString).getOrElse("No Result")
    println(solution1)
    val solution2 = solveUsingSortAndLoop(desiredSum, "day01-input.txt").map(_.toString).getOrElse("No Result")
    println(solution2)
  }

  private def solveUsingHashSet(desiredSum: Int, fileName: String): Option[Solution] = {
    val lines: Set[Int] = Source.fromResource(fileName).getLines().map(_.toInt).toSet

    val matchingResults = for {
      input <- Source.fromResource(fileName).getLines().map(_.toInt)
      num = input.toInt
      if (lines.contains(desiredSum - num))
    } yield Solution(num, desiredSum-num)

    matchingResults.toList.headOption
  }

  private def solveUsingSortAndLoop(desiredSum: Int, fileName: String): Option[Solution] = {
    val lines: List[Int] = Source.fromResource(fileName).getLines().map(_.toInt).toList.sorted

    var i = 0
    var j = lines.size-1

    while(i <= lines.size-1 && j >= 0) {
      val currentSum = lines(i) + lines(j)
      if(currentSum == desiredSum)
        return Some(Solution(lines(i), lines(j)))
      else if (currentSum < desiredSum)
        i = i + 1
      else
        j = j - 1
    }
    None
  }


}


