package day09

import scala.io.Source

object EncodingError {

  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day09-input01.txt")
    val input2 = parseInput("day09-input02.txt")

    val num1: Long = findWrongNumber(5, 5, input1)
    println(s"Wrong number  = $num1")

    val num2: Long = findWrongNumber(25, 25, input2)
    println(s"Wrong number  = $num2")

    val cset1 = findContigousSet(0, num1, input1)
    println(s"Contigous Set = $cset1, Min = ${cset1.min}, Max = ${cset1.max}")

    val cset2 = findContigousSet(0, num2, input2)
    println(s"Contigous Set = $cset2, Min = ${cset2.min}, Max = ${cset2.max}")

  }


  def parseInput(fileName: String): List[Long] = {
    val source: Source = Source.fromResource(fileName)
    val lines: List[Long] = source.getLines().map(_.toLong).toList
    source.close()
    lines
  }


  def findWrongNumber(n:Int, currentPos:Int, numbers: List[Long]): Long = {
    if(currentPos >= numbers.size){
      return 0
    }
    val preamble: List[Long] = numbers.take(currentPos).takeRight(n).sorted

    var i = 0
    var j = preamble.size - 1
    var matchFound = false
    while(i < j && !matchFound) {
      val sum = preamble(i) + preamble(j)
      if(sum == numbers(currentPos))
        matchFound = true
      else if (sum > numbers(currentPos))
        j = j - 1
      else
        i = i + 1
    }
    if(matchFound)
      findWrongNumber(n, currentPos + 1, numbers)
    else
      numbers(currentPos)
  }


  def findContigousSet(start: Int, num: Long, list: List[Long]): List[Long]= {
    var currentSum = list(start)
    var found = false
    var i = start
    var j = i + 1
    while(!found && j < list.size) {
      currentSum = currentSum + list(j)
      if(currentSum == num) {
        found = true
      }
      else if(currentSum < num) {
        j = j + 1
      }else {
        i = i + 1
        j = i + 1
        currentSum = list(i)
      }

    }
    if(found) {
      list.slice(i, j+1)
    } else {
      List()
    }
  }
}
