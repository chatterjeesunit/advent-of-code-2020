package day10

import scala.io.Source

object Test {

  var possiblePaths: Map[Int, Int] =
    Map(
      1 -> 1,
      2 -> 2,
      3 -> 4
    )


  def getPossiblePaths(key: Int): Int = {
    possiblePaths.get(key) match {
      case Some(value) => value
      case _ => {
        val result1 = getPossiblePaths( key-3)
        val result2 = getPossiblePaths(key-2)
        val result3 = getPossiblePaths(key-1)

        possiblePaths += (key -> ( result1 + result2 + result3))
        result1 + result2 + result3
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day10-input01.txt")
    val input2 = parseInput("day10-input02.txt")
    val input3 = parseInput("day10-input03.txt")

    println("\n\n****** Find Jolts - Input 1 **************")
    val (x1, y1) = findJolts(input1)
    println(s"1 Diff count = $x1, 3 Diff count = $y1, Result = ${x1*y1}")


    println("\n\n****** Find Jolts - Input 2 **************")
    val (x2, y2) = findJolts(input2)
    println(s"1 Diff count = $x2, 3 Diff count = $y2, Result = ${x2*y2}")


    println("\n\n****** Find Jolts - Input 3 **************")
    val (x3, y3) = findJolts(input3)
    println(s"1 Diff count = $x3, 3 Diff count = $y3, Result = ${x3*y3}")

    println("\n\n****** Find Maximum Paths - Input 1 **************")
    val paths1 = findDistinctPaths(input1)
    println(s"Total Paths = $paths1")

    println("\n\n****** Find Maximum Paths - Input 2 **************")
    val paths2 = findDistinctPaths(input2)
    println(s"Total Paths = $paths2")

    println("\n\n****** Find Maximum Paths - Input 3 **************")
    val paths3 = findDistinctPaths(input3)
    println(s"Total Paths = $paths3")



  }


  def parseInput(fileName: String): List[Long] = {
    val source: Source = Source.fromResource(fileName)
    val lines: List[Long] = source.getLines().map(_.toLong).toList.sorted
    source.close()
    (0l :: lines) ++ List(lines(lines.size - 1) + 3l)
  }

  def findJolts(input: List[Long]): (Int, Int) = {
    println(s"Input Data Sorted = $input")
    val result:Map[Long, Int] = input.sliding(2).toList.map(x => x(1) - x(0)).groupBy(identity).view.mapValues(_.size).toMap
    (result.get(1l).getOrElse(0), result.get(3l).getOrElse(0))
  }




  def findDistinctPaths(input: List[Long]) : Long = {
    println(s"Input Data Sorted = $input")
    val slidingList = input.sliding(2).toList.map(x => x(1) - x(0))

    var i = 0
    val countOfConsecutiveOnes: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map()
    var oneCount = 0;
        println(s"Sliding List of Consecutive Diff = $slidingList")
    while(i < slidingList.size) {
      if(slidingList(i) == 1) {
        oneCount = oneCount + 1
      }
      else {
        if(oneCount > 1) {
          countOfConsecutiveOnes.put(oneCount, countOfConsecutiveOnes.get(oneCount).getOrElse(0)+1)
        }
        oneCount = 0
      }
      i = i + 1
    }
    val totalPaths = countOfConsecutiveOnes.map ( m => {
      possiblePaths.get(m._1) match {
        case Some(v) => Math.pow(v , m._2)
        case _ => {
          val paths = getPossiblePaths(m._1)
          Math.pow(paths , m._2)
        }
      }
    }).reduce((x,y) => x*y).toLong

    println(s"Count of Consecutive One Jolt Diffs = ${countOfConsecutiveOnes}")
    println(s"Total Paths = ${countOfConsecutiveOnes.map(m=> s"(${possiblePaths.get(m._1).get} ^ ${m._2})").toList.mkString(" * ")}")

    totalPaths
  }
}
