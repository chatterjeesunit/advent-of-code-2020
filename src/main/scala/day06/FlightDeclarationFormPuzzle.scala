package day06

import scala.io.{BufferedSource, Source}

/**
 * https://adventofcode.com/2020/day/6
 *
 */
object FlightDeclarationFormPuzzle {

  case class Data(input: Array[String]) {
    def numberOfPersons:Int = input.length
    def getResponses: Map[Char, Int] =
        input
          .flatMap(_.toList)
          .groupBy(identity)
          .map(t => (t._1, t._2.length))
  }

  def defaultValidCountFunction(input: Data):Int = input.getResponses.keySet.size
  def allYesValidCountFunction (input: Data):Int = input.getResponses.filter(m => m._2 == input.numberOfPersons).size

  def main(args: Array[String]): Unit = {

    val count1:Int = getValidResponses("day06-input01.txt", defaultValidCountFunction)
    println(s"Puzzle 1 - Input 1 - Total Count = $count1")

    val count2:Int = getValidResponses("day06-input02.txt", defaultValidCountFunction)
    println(s"Puzzle 1 - Input 2 - Total Count = $count2")

    val count3:Int = getValidResponses("day06-input01.txt", allYesValidCountFunction)
    println(s"Puzzle 2 - Input 1 - Total Count = $count3")

    val count4:Int = getValidResponses("day06-input02.txt", allYesValidCountFunction)
    println(s"Puzzle 2 - Input 2 - Total Count = $count4")
  }


  def getValidResponses(fileName: String, validCountFunction: Function[Data, Int]):Int = {

    val source: BufferedSource = Source.fromResource(fileName)
    val d: List[Data] =
        source.getLines()
          .mkString("|")
          .split("\\|{2,}")
          .map(s => Data(s.split("\\|")))
          .toList

    source.close()
    d.map(d1 => validCountFunction.apply(d1)).sum
  }

}
