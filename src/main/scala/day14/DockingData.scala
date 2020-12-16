package day14

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
/*
https://adventofcode.com/2020/day/14

 */
object DockingData {

  case class Mem(pos: Int, value:Int)


  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day14-input01a.txt")
    val input2 = parseInput("day14-input02.txt")
    val input3 = parseInput("day14-input01b.txt")

//    println("******* Part 1 - Test Input 1 ************")
//    val result1 = part1(input1)
//    println(result1)
//
//    println("\n******* Part 1 - Puzzle 1 Input ************")
//    val result2 = part1(input2)
//    println(result2)

    println("\n\n******* Part 2 - Test Input 1 ************")
    val result3 = part2(input3)
    println(result3)

    println("\n\n******* Part 2 - Puzzle 2 Input ************")
    val result4 = part2(input2)
    println(result4)
  }

  def binaryToInteger(row: String): Long = {
    row
      .reverse
      .split("")
      .zipWithIndex
      .map(x => (x._1.toLong, x._2))
      .filter(_._1 == 1)
      .map(x => Math.pow(2,x._2).toLong)
      .sum
  }

  def IntegerToBinary(num: Long, bitSize: Int): String = {
    val t0 = "0".padTo(bitSize, '0')
    val t1 = num.toBinaryString
    t0.take(bitSize-t1.length) + t1
  }

  def part1(lines: List[String]): Long = {
    val maskRegex = "mask = ([X10]{36})".r
    val memRegex = "mem\\[([0-9]+)\\]\\s=\\s([0-9]+)".r

    val map: scala.collection.mutable.Map[Int, Long] = scala.collection.mutable.Map()
    var mask0:Long = 0l
    var mask1: Long = 0l
    lines.foreach(line => {
       line match {
         case maskRegex(m) => {
           // for Zero overwrite - replace all X with 1 and AND with number
           mask0 = binaryToInteger(m.replaceAll("X", "1"))
           // for 1 overwrite - replace all X with 0 and OR with number
           mask1 = binaryToInteger(m.replaceAll("X", "0"))
         }
         case memRegex(pos, number) => {
           val newNum:Long = ((number.toLong & mask0) | mask1)
           map.put(pos.toInt, newNum)
         }
       }
    })

    map.values.sum
  }


  def part2(lines: List[String]): Long = {
    val maskRegex = "mask = ([X10]{36})".r
    val memRegex = "mem\\[([0-9]+)\\]\\s=\\s([0-9]+)".r

    val result: scala.collection.mutable.Map[Long, Long] = scala.collection.mutable.Map()
    var mask: String = ""
    lines.foreach(line => {
      line match {
        case maskRegex(m) => {
          mask = m
          println(mask)
        }
        case memRegex(pos, number) => {
          val numBinary = new ListBuffer().addAll(IntegerToBinary(pos.toLong, 36).toList)
          mask.zipWithIndex.foreach( c => {
            if(c._1 == 'X' || c._1 == '1') numBinary(c._2) = c._1
          })

          val maskedStr: String = numBinary.toList.mkString("")
          println("\tMask = " + maskedStr)
          combinations(maskedStr, List("")).foreach( n => {
            result.put(binaryToInteger(n), number.toLong)
          })
        }
      }
//      println(result)
    })


    result.toMap.values.sum
  }



  def parseInput(fileName: String)= {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  @tailrec
  def combinations(str: String, result: List[String]): List[String] = {
    val i = str.indexOf("X")
    if(i == -1) {
      return result
    }
    val prefix = str.slice(0,i)
    val newResult = result.flatMap(r =>
      List(r + prefix + "0", r + prefix + "1")
    )
    if(i+1 >= str.length) {
      return newResult
    }
//    println(s"\t\t\t ${newResult.mkString("\n\t\t\t")}")
    combinations(str.slice(i+1,str.length), newResult)
  }

}
