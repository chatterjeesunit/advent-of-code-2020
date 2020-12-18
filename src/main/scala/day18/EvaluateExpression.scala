package day18

import scala.collection.mutable
import scala.io.Source

/*
https://adventofcode.com/2020/day/18
List(71, 51, 26, 437, 12240, 13632)
Sum = 26457

Sum = 11297104473091

List(231, 51, 46, 1445, 669060, 23340)
Sum = 694173

Sum = 185348874183674


 */
object EvaluateExpression {

  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day18-input1.txt")
    val input2 = parseInput("day18-input2.txt")

    println("******* Part 1 - Test ************")
    val result1 = part1(input1)
    println(result1)
    println(s"Sum = ${result1.sum}")

    println("\n******* Part 1 - Puzzle ************")
    val result2 = part1(input2)
    println(result2)
    println(s"Sum = ${result2.sum}")

    println("\n\n******* Part 2 - Test ************")
      val result3 = part2(input1)
      println(result3)
      println(s"Sum = ${result3.sum}")

    println("\n\n******* Part 2 - Puzzle ************")
    val result4 = part2(input2)
    println(result4)
    println(s"Sum = ${result4.sum}")

  }



  def parseInput(fileName: String): List[String] = {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()
    lines
  }

  def part1(input: List[String]) = {
    val precedence: Map[String, Int] = Map(
      "(" -> 0,
      "*" -> 1,
      "+" -> 1
    )
    evaluateExpression(input, precedence)
  }

  def part2(input: List[String]) = {
    val precedence: Map[String, Int] = Map(
      "(" -> 1,
      "*" -> 2,
      "+" -> 3
    )
    evaluateExpression(input, precedence)
  }

  private def evaluateExpression(input: List[String], precedence: Map[String, Int]) = {
    input.map(line => {
      val postFixString: String = parseToPostFix(line, precedence)
//      println(postFixString)
      val result = eval(postFixString)
//      println(result)
      result
    })
  }



  def parseToPostFix(str: String, precedence: Map[String, Int]): String = {
    val number = "([0-9]*)".r
    val opStack: mutable.Stack[String] = mutable.Stack()
    val buffer: mutable.StringBuilder = new StringBuilder()
    str.toList.map(_.toString match {
      case number(n) => buffer.append(n)
      case "(" => opStack.push("(")
      case ")" => {
        var lastOp = opStack.pop()
        while(lastOp != "(") {
          buffer.append(lastOp)
          lastOp = opStack.pop()
        }
      }
      case op if op.equals("*") || op.equals("+") => {
        if(!opStack.isEmpty) {
          val prevOp = opStack.top
          if(precedence.getOrElse(prevOp, 0) >= precedence.getOrElse(op, 0)) {
            buffer.append(opStack.pop)
          }
        }
        opStack.push(op)
      }
      case _ => //Do Nothing
    })

    while(!opStack.isEmpty) {
      buffer.append(opStack.pop())
    }

    buffer.toString()
  }

  def eval(postFix: String): Long = {
    val number = "([0-9]*)".r
    val numStack: mutable.Stack[Long] = mutable.Stack()
    postFix.toList.foreach(_.toString match {
      case number(n) => numStack.push(n.toLong)
      case "*" => {
        val newNum = numStack.pop() * numStack.pop()
        numStack.push(newNum)
      }
      case "+" => {
        val newNum = numStack.pop() + numStack.pop()
        numStack.push(newNum)
      }
    })
    numStack.pop()
  }
}
