package day19

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/*
https://adventofcode.com/2020/day/19
2
120

3
12
350


 */
object Test {

  case class Rule(ruleText: Option[String], otherRules: List[List[Int]]) {
    def isRuleValid(): Boolean = ruleText.isDefined
  }

  def main(args: Array[String]): Unit = {
    val (rules1a, messages1a) = parseInput("day19-input01a.txt")
    val parsedRules1a = parseRules(rules1a)

    val (rules1b, messages1b) = parseInput("day19-input01b.txt")
    val parsedRules1b = parseRules(rules1b)

    val (rules2, messages2) = parseInput("day19-input02.txt")
    val parsedRules2 = parseRules(rules2)



    println("******* Part 1 - Test ************")
    val result1a = part1(messages1a, parsedRules1a, 0)
    println(s"Number of matched rules = ${result1a}")

    println("\n******* Part 1 - Puzzle ************")
    val result2 = part1(messages2, parsedRules2, 0)
    println(s"Number of matched rules = ${result2}")

    println("\n\n******* Part 2 - Test ************")
    val result1b = part1(messages1b, parsedRules1b, 0)
    println(s"Number of matched rules  = ${result1b}")
    val result3 = part2(messages1b, parsedRules1b)
    println(s"Number of matched rules (after changing regex) = ${result3}")

    println("\n\n******* Part 2 - Puzzle ************")
    val result4 = part2(messages2, parsedRules2)
    println(s"Number of matched rules (after changing regex) = ${result4}")


  }


  def part1(messages: List[String], rules: Map[Int, String], ruleNumber: Int): Int = {
    val ruleRegex = rules.get(ruleNumber).get.r
//    println(rules.get(ruleNumber).get)
    val filteredMessages = messages.filter(ruleRegex.matches(_))
//    println(s"\t${filteredMessages.mkString("\n\t")}")
    filteredMessages.size
  }


  def part2(messages: List[String], rules: Map[Int, String]): Int = {
    val rule42original = rules.get(42).get
    val rule31original = rules.get(31).get

    def evalRecursiveRegex(msg: List[String], rule11: String,  count: Int): Int = {
      val newRule8 = s"(${rule42original})+"
      val newRule11 = s"($rule42original)($rule11)($rule31original)"
      val regex = s"($newRule8)($newRule11)".r

      val(matched, unmatched) = msg.partition(s => regex.matches(s))
      if(unmatched.size == msg.size) matched.size + count
      else evalRecursiveRegex(unmatched, newRule11, count + matched.size)
    }
    evalRecursiveRegex(messages, "", 0)
  }

  def parseRules(inputRules: Map[Int, Rule]): Map[Int, String] = {

    val finalRuleMap =  mutable.Map[Int, String]()

    def evalRule(rnum: Int, rule: Rule): String = {
      val ruleValue =
        rule.otherRules.map( rlist =>
          rlist.map(n => {
              finalRuleMap.get(n) match {
                case Some(ruleText) => ruleText
                case _ => {
                  val rtext = evalRule(n, inputRules.get(n).get)
                  finalRuleMap.put(n, rtext)
                  rtext
                }
              }
            }).reduce(_ + _)
        ) match {
          case l :: List() => l
          case l1::l2::List() => s"(${l1}|${l2})"
          case _ => ""
        }

      finalRuleMap.put(rnum, ruleValue)

      ruleValue
    }

    val (validRules, invalidRules) = inputRules.partition(_._2.isRuleValid())
    validRules.foreach(r => finalRuleMap.put(r._1, r._2.ruleText.get))

    invalidRules.foreach(r => evalRule(r._1, r._2))

    finalRuleMap.toMap
  }


  def parseInput(fileName: String) = {
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()

    val textRegex = "\"([a-z]+)\"".r

    val rules = mutable.Map[Int, Rule]()
    val messages = ListBuffer[String]()
    var matchRules = true
    lines.foreach(l => {
        if(l.length == 0) matchRules = false
        else if(matchRules) {
          val arr = l.split(":")
          val ruleNumber = arr(0)
          val newRule = arr(1).trim match {
            case textRegex(t) => Rule(Some(t), List())
            case _ => {
              val rules = arr(1).trim.split("\\|").toList.map(_.trim.split("\\s").map(_.toInt).toList)
              Rule(None, rules)
            }
          }
          rules.put(ruleNumber.toInt, newRule)
        } else {
          messages.append(l)
        }
    })
    (rules.toMap, messages.toList)
  }

}
