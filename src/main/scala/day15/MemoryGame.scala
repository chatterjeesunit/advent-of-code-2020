package day15

import scala.io.Source
/*
https://adventofcode.com/2020/day/15
436, 1, 10, 27, 78, 438, 1836
694
175594, 2578, 3544142, 261214, 6895259, 18, 362
21768614
 */
object MemoryGame {
  case class Chance(lastChance: Int = 0, previousChance: Int = 0)


  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day15-input01.txt")
    val input2 = parseInput("day15-input02.txt")

    println("******* Part 1 - Test Input 1 ************")
    val result1 = part1(input1, 2020)
    println(result1)

    println("\n******* Part 1 - Puzzle 1 Input ************")
    val result2 = part1(input2, 2020)
    println(result2)

    println("\n\n******* Part 2 - Test Input 1 ************")
    val result3 = part1(input1, 30000000)
    println(result3)

    println("\n\n******* Part 2 - Puzzle 2 Input ************")
    val result4 = part1(input2, 30000000)
    println(result4)
  }


  def part1(lines: List[List[Int]], limit: Int) = {


    lines.map(l => findNthNumberSpoken(l, limit))
  }


  def findNthNumberSpoken(numbers: List[Int], limit: Int) = {
    var currentChance = 0
    var lastNumberSpoken: Int = -1
    val map: scala.collection.mutable.Map[Int, Chance] = scala.collection.mutable.Map()
    numbers.foreach(num => {
      currentChance = currentChance + 1
      map.put(num, Chance(currentChance, currentChance))
      lastNumberSpoken = num
    })

    while (currentChance < limit) {
      currentChance = currentChance + 1
      lastNumberSpoken = map.get(lastNumberSpoken) match {
        case Some(c) => c.lastChance - c.previousChance
        case _ => 0
      }
      map.put(lastNumberSpoken, map.get(lastNumberSpoken) match {
        case Some(c) => c.copy(previousChance = c.lastChance, lastChance = currentChance)
        case _ => Chance(currentChance, currentChance)
      })

    }
    lastNumberSpoken
  }


  def parseInput(fileName: String)= {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().map(_.split(",").map(_.toInt).toList).toList
    source.close()

    lines
  }

}
