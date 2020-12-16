package day16

import scala.io.Source
/*
https://adventofcode.com/2020/day/16
71

24110

seat -> 13, row -> 11, class -> 1

departure location -> 173, departure date -> 109, departure track -> 239, departure platform -> 73, departure time -> 157, departure station -> 131
6766503490793
 */
object DecodeTickets {

  case class Range(low: Int, high: Int) {
    def isInRange(num: Int) = num >=low && num <= high
  }
  case class Rule(name: String, rangeOne: Range, rangeTwo: Range) {
    def isRuleMatches(num: Int) = rangeOne.isInRange(num) || rangeTwo.isInRange(num)
  }
  case class InputData(rules: List[Rule], myTickets: List[Int], nearbyTickets: List[List[Int]])

  case class PossibleValues(pos: Int, fields: List[String])

  def main(args: Array[String]): Unit = {
    val input1a = parseInput("day16-input01-a.txt")
    val input1b = parseInput("day16-input01-b.txt")
    val input2 = parseInput("day16-input02.txt")

    println("******* Part 1 - Test Input 1a ************")
    val result1 = input1a.nearbyTickets.flatten.filterNot(n => input1a.rules.exists(_.isRuleMatches(n))).sum
    println(result1)

    println("\n******* Part 1 - Puzzle 1 Input ************")
    val result2 = input2.nearbyTickets.flatten.filterNot(n => input2.rules.exists(_.isRuleMatches(n))).sum
    println(result2)

    println("\n\n******* Part 2 - Test Input 1b ************")
    val result3 = readTicketValues(input1b)
    println(result3)

    println("\n\n******* Part 2 - Puzzle 2 Input ************")
    val result4 = readTicketValues(input2)
    val filteredKeys = Set("departure location", "departure date", "departure track", "departure platform", "departure time", "departure station")
    println(result4.filter(m => filteredKeys.contains(m._1)))
    println(result4.filter(m => filteredKeys.contains(m._1)).values.map(_.toLong).product)
  }


  private def readTicketValues(input: InputData) = {
    val filteredTickets =
      input
      .nearbyTickets
        .filter(tickets =>
          tickets.forall(num =>
            input.rules.exists(_.isRuleMatches(num))
          )
        )

//    println(filteredTickets)
    val columns = filteredTickets(0).size
    val rows = filteredTickets.size
    val finalListOfPossibleValues =
      for {
        col <- 0 to columns - 1
      } yield {
        val result = for {
          row <- 0 to rows - 1
        } yield {
          val num = filteredTickets(row)(col)
          input.rules.filter(_.isRuleMatches(num)).map(_.name)
        }
      val counts = result.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      val possibleValues = counts.filter(c => c._2 == rows).map(_._1).toList
      PossibleValues(col, possibleValues)
    }
//    println(finalListOfPossibleValues.toList.mkString("\n"))
    val fieldPositions = findAllPositions(finalListOfPossibleValues.toList, Map(), columns)
    fieldPositions.map( m => m._1 -> input.myTickets(m._2))
  }

  def findAllPositions(input: List[PossibleValues], result: Map[String, Int], colCount: Int) : Map[String, Int] = {
    if(colCount == result.size)
      return result

    val matched = input.map(pv => (pv.pos, pv.fields.filterNot(f => result.contains(f)))).filter(_._2.size == 1)(0)

//    println(matched)

    findAllPositions(input, result + (matched._2(0) -> matched._1), colCount)
  }

  def parseInput(fileName: String): InputData = {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()

    val rulesRegex = "([\\w\\s]+):\\s([\\d]+)-([\\d]+)\\sor\\s([\\d]+)-([\\d]+)".r
    lines.span(s => s.equals("\n"))

    val index = lines.indexWhere(s => s.equals(""))

    val rulesList: List[Rule] = lines.take(index).map(_ match {
      case rulesRegex(ruleName, rangeOneLow, rangeOneHigh, rangeTwoLow, rangeTwoHigh) =>
        Rule(ruleName, Range(rangeOneLow.toInt, rangeOneHigh.toInt), Range(rangeTwoLow.toInt, rangeTwoHigh.toInt ))
    })

    val myTickets: List[Int] = lines(index+2).split(",").map(_.toInt).toList

    val nearbyTickets: List[List[Int]] = lines.drop(index + 5).map(_.split(",").map(_.toInt).toList).toList


    InputData(rulesList, myTickets, nearbyTickets)
  }

}
