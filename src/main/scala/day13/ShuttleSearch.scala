package day13

import scala.io.Source
/*
https://adventofcode.com/2020/day/13
295
207


3417
754018
779210
1261476
1202161486
1068781
530015546283687
 */
object ShuttleSearch {

  case class Shuttle(busId: Long, timeDiff:Int)

  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day13-input01.txt")
    val input2 = parseInput("day13-input02.txt")

    println("******* Part 1 - Input 1 ************")
    findEarliestBusAfterGivenTime(input1)

    println("\n******* Part 1 - Input 2 ************")
    findEarliestBusAfterGivenTime(input2)


    println("\n\n******* Part 2 - All Inputs************")

    val input3 = parseShuttles("day13-input03.txt")
    input3.foreach(i => test22(i))
  }


  private def findEarliestBusAfterGivenTime(input: (String, List[Shuttle])) = {
    val (startTime: Int, shuttles:List[Shuttle]) = (input._1.toInt, input._2)
    println(s"Starting Time = ${startTime}, Shuttles = ${shuttles}")
    val (shuttle, time) = shuttles
      .map(bus => (bus, bus.busId * Math.ceil(input._1.toDouble / bus.busId).toInt))
      .filter(result => result._2 >= startTime)
      .sortBy(_._2)
      .head
    println(s"Shuttle = ${shuttle.busId}, Time = ${time}")
    println(s"Solution = ${ (time - startTime) * shuttle.busId}")
  }

  private def test22(input: List[Shuttle]) = {
    println(input)
    var time: Long = input(0).busId
    var advanceBy:Long = time
    var i = 1
    while (i < input.size)
    {
      var break = false
      while(!break )
      {
        if ((time + input(i).timeDiff ) % input(i).busId != 0)
          time = time + advanceBy
        else
        {
          break = true
          advanceBy = lcm(Seq(advanceBy, input(i).busId))
        }
      }
      i = i + 1
    }


    println(s"Solution = $time")
    println("-----------------")
  }



  def parseInput(fileName: String): (String, List[Shuttle]) = {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()

    ( lines(0), getListOfShuttles(lines.takeRight(1)).head )


  }

  def parseShuttles(fileName: String): List[List[Shuttle]]= {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()
    getListOfShuttles(lines)
  }

  private def getListOfShuttles(lines: List[String]) = {
    lines.map(_.split(",")
      .toList
      .zipWithIndex
      .filter(!_._1.equals("x"))
      .map(b => Shuttle(b._1.toLong, b._2)))
  }

  def lcm(list: Seq[Long]):Long=list.foldLeft(1:Long){
    (a, b) => b * a /
      Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
  }
}
