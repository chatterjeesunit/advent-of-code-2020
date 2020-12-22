package day20

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

/*
https://adventofcode.com/2020/day/20
20899048083289

 */
object Monster {


  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day20-input01.txt")
    val input2 = parseInput("day20-input02.txt")



    println("******* Part 1 - Test ************")
    val result1 = part1(input1)
    println(result1)

    println("\n******* Part 1 - Puzzle ************")
    val result2 = part1(input2)
    println(result2)

    println("\n\n******* Part 2 - Test ************")

    println("\n\n******* Part 2 - Puzzle ************")


  }

  @tailrec
  def findHorizontalMatches(input: Array[Tile], result: ListBuffer[Tile], n:Int): ListBuffer[Tile] = {
    if(input.length == 0) {
      return result
    }
    var matchFound = false
    var i = 1
    var newTile = input(0)
    while(!matchFound && i < input.length){
      newTile.matchHorizontal(input(i)) match {
        case Some(t) => {
          newTile = t
          matchFound = true
        }
        case None => i = i + 1
      }
    }

    val newArr = swap(input, 1, i)
    val newInput = if (newTile.isTileLengthMaxed(n)) {
      result.append(newTile)
      newArr.drop(2)
    }else {
      newTile +: newArr.drop(2)
    }

    findHorizontalMatches(newInput, result, n)
  }

  @tailrec
  def findVerticalMatches(input: Array[Tile], result: ListBuffer[Tile], n:Int): ListBuffer[Tile] = {
    if(input.length == 0) {
      return result
    }
    var matchFound = false
    var i = 1
    var newTile = input(0)
    while(!matchFound && i < input.length){
      newTile.matchVertical(input(i)) match {
        case Some(t) => {
          newTile = t
          matchFound = true
        }
        case None => i = i + 1
      }
    }

    val newArr = swap(input, 1, i)
    val newInput = if (newTile.isTileHieghtMaxed(n)) {
      result.append(newTile)
      newArr.drop(2)
    }else {
      newTile +: newArr.drop(2)
    }

    findVerticalMatches(newInput, result, n)
  }

  def swap(arr: Array[Tile], i:Int, j:Int): Array[Tile] = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    arr
  }

  def part1(input: Array[Tile]): Long = {


    val dimension = Math.sqrt(input.size).toInt
    val result = findHorizontalMatches(input, ListBuffer(), dimension).toArray
    println(result.toList)
    val finalResult = findVerticalMatches(result, ListBuffer(), dimension)
    println(finalResult.toList)

    val arr = finalResult.toList.map(_.tileId)(0).split(";").map(_.split(","))
    println(s"${arr(0)(0)}, ${arr(0)(dimension-1)}, ${arr(dimension-1)(0)}, ${arr(dimension-1)(dimension-1)}")

    arr(0)(0).toLong * arr(0)(dimension-1).toLong * arr(dimension-1)(0).toLong * arr(dimension-1)(dimension-1).toLong
  }

  def parseInput(fileName: String): Array[Tile] = {
    val tileIdRegex = "Tile ([0-9]+):".r
//    val tileRegex = "(\\.#)+".r
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList.mkString("|")
    println(lines)
    source.close()
    lines.split("\\|\\|").map( t => {
      val index = t.indexOf("|")
      val tileIdRegex(tileId) = t.take(index)
      val tileData = t.slice(index+1, t.length).split("\\|").toList
      val top = tileData(0)
      val bottom = tileData(tileData.size - 1)

      val left = for (i <- 0 to tileData.size - 1) yield { tileData(i)(0) }
      val right = for (i <- 0 to tileData.size - 1) yield { tileData(i)(tileData.size - 1) }
      Tile(tileId, top, bottom, left.mkString(""), right.mkString(""))
    })
  }

}
