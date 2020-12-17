package day17

import scala.collection.mutable.{Set => mSet}
import scala.io.Source
/*
https://adventofcode.com/2020/day/17
112
273
848
1504


 */
object ConwayCubes {

  def main(args: Array[String]): Unit = {
    val (input1, row1, col1) = parseInput("day17-input01.txt")
    val (input2, row2, col2) = parseInput("day17-input02.txt")

    println("******* Part 1 - Test ************")
    val result1 = rounds(
                    input1, 1, 6,
                    HyperCoordinate(0,0,0,0), HyperCoordinate(row1, col1, 0, 0), false).size
    println(s"Total active cubes = ${result1}")

    println("\n******* Part 1 - Puzzle ************")
    val result2 = rounds(
                    input2  , 1, 6,
                    HyperCoordinate(0,0,0,0), HyperCoordinate(row2, col2, 0, 0), false).size
    println(s"Total active cubes = ${result2}")

    println("\n\n******* Part 2 - Test ************")
    val result3 = rounds(
                    input1, 1, 6,
                    HyperCoordinate(0,0,0,0), HyperCoordinate(row1, col1, 0, 0), true).size
    println(s"Total active cubes = ${result3}")

    println("\n\n******* Part 2 - Puzzle ************")
    val result4 = rounds(
                    input2, 1, 6,
                    HyperCoordinate(0,0,0,0), HyperCoordinate(row2, col2, 0, 0), true).size
    println(s"Total active cubes = ${result4}")

  }



  def parseInput(fileName: String): (Set[HyperCoordinate], Int, Int) = {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()
    val activePos: mSet[HyperCoordinate] = mSet()
    var row = 0
    lines.foreach(l => {
      l.zipWithIndex.foreach( c => if(c._1 == '#') activePos.add(HyperCoordinate(row, c._2, 0, 0)))
      row = row + 1
    })
//    println(activePos)
    (activePos.toSet, row, row)
  }


  def rounds(activePos: Set[HyperCoordinate], counter: Int, n:Int,
             start: HyperCoordinate, end: HyperCoordinate, checkFAxis: Boolean): Set[HyperCoordinate] = {
    if(counter > n) {
      return activePos
    }

    val newActivePos: mSet[HyperCoordinate] = mSet()
    val coordinates = for {
      x <- (start.x - counter) to (end.x + counter)
      y <- (start.y - counter) to (end.y + counter)
      z <- (start.z - counter) to (end.z + counter)
      f <- if(checkFAxis) (start.f - counter) to (end.f + counter) else 0 to 0
    } yield {
      HyperCoordinate(x, y, z, f)
    }

    coordinates.foreach(c => {
      val activeN = getActiveNeighbours(activePos, c, checkFAxis)
      if(activePos.contains(c)) {
        if(activeN == 2 || activeN == 3) {
          newActivePos.add(c)
        }
      } else if(activeN == 3) {
        newActivePos.add(c)
      }
    })
    //      println(newActivePos)
    //      println(s"Total Active Cubes = ${newActivePos.size}")
    rounds(newActivePos.toSet, counter+1, n, start, end, checkFAxis)
  }


  def getActiveNeighbours(activePos: Set[HyperCoordinate], coordinate: HyperCoordinate, checkFAxis: Boolean): Int = {
    val allcubes = for {
      x <- coordinate.x-1 to coordinate.x+1
      y <- coordinate.y-1 to coordinate.y+1
      z <- coordinate.z-1 to coordinate.z+1
      f <- if(checkFAxis) coordinate.f-1 to coordinate.f+1 else 0 to 0
    } yield {
      val newCordinate = HyperCoordinate(x, y, z, f)
      if(newCordinate.isNeighbour(coordinate )) Some(newCordinate) else None
    }

    val activeNeighbours = allcubes.flatten.filter(c => activePos.contains(c))

    activeNeighbours.count(_ => true)
  }

}
