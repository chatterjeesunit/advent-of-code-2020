package day08

import scala.io.Source

object FixGame {
  case class Instruction(command:String, num: Int)

  def main(args: Array[String]): Unit = {

    val inputArr1: List[Instruction] = parseInput("day08-input01.txt")
    val inputArr2: List[Instruction] = parseInput("day08-input02.txt")

    findAccumulatorValueBeforeInfiniteLoop(inputArr1)
    findAccumulatorValueBeforeInfiniteLoop(inputArr2)

    fixInfiniteLoop(inputArr2)
  }

  def parseInput(fileName: String): List[Instruction] = {
    val source: Source = Source.fromResource(fileName)
    val result: List[Instruction] = source.getLines().flatMap(line =>
      line.split("\\s") match {
        case Array(command, num) => Some(Instruction(command, num.toInt))
        case _ => None
      }
    ).toList
    source.close();
    result
  }

  def findAccumulatorValueBeforeInfiniteLoop(inputArr: List[Instruction]) = {
    val arr = Array.fill(inputArr.size)(0)

    var i: Int = 0
    var acc: Int = 0
    var runLoop: Boolean = true
    var counter: Int = 0

    while (runLoop) {
      counter = counter + 1
      arr(i) = counter
      inputArr(i) match {
        case Instruction("acc", n) => {
          i = i + 1
          acc = acc + n
        }
        case Instruction("nop", _) => i = i + 1
        case Instruction("jmp", n) => i = i + n
        case _ => runLoop = false
      }
      if (arr(i) > 0 || i >= arr.length) {
        runLoop = false
      }
    }

    println(s"i=$i, counter=$counter, acc=$acc")
    (i, counter, acc)
  }

  def fixInfiniteLoop(inputArr: List[Instruction]) = {
    var changeInstructionPos: Set[Int] = Set()
    var isFixed = false
    while(!isFixed) {
      val (i, _, acc, newInstructionChangedPos) = runTestFix(inputArr, changeInstructionPos)
      changeInstructionPos = newInstructionChangedPos
      if(i >= inputArr.length) {
        println(s"i = $i, acc = $acc, Instruction Position = ${newInstructionChangedPos.takeRight(1).head}, Instruction Fixed = ${inputArr(newInstructionChangedPos.takeRight(1).head)}")
        isFixed = true
      }
    }

    def runTestFix(inputArr: List[Instruction], changedInstructionPos: Set[Int]) = {
      val arr = Array.fill(inputArr.size)(0)
      var oneInstructionChanged = false
      var newInstructionChangedPos = changeInstructionPos

      var i: Int = 0
      var acc: Int = 0
      var runLoop: Boolean = true
      var counter: Int = 0

      while (runLoop) {
        counter = counter + 1
        arr(i) = counter
        inputArr(i) match {
          case Instruction("acc", n) => {
            i = i + 1
            acc = acc + n
          }
          case Instruction("nop", n) => {
            if(!changedInstructionPos.contains(i) && oneInstructionChanged==false && n != 0) {
              newInstructionChangedPos = changeInstructionPos++Set(i)
              i = i + n
              oneInstructionChanged = true
            } else {
              i = i + 1
            }

          }
          case Instruction("jmp", n) => {
            if(!changedInstructionPos.contains(i) && oneInstructionChanged==false) {
              newInstructionChangedPos = changeInstructionPos++Set(i)
              i = i + 1
              oneInstructionChanged = true
            } else {
              i = i + n
            }
          }
          case _ => {
            runLoop = false
          }
        }
        if (i >= arr.length || arr(i) > 0) {
          runLoop = false
        }
      }

      (i, counter, acc, newInstructionChangedPos)
    }
  }

}
