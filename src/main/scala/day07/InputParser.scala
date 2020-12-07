package day07

import scala.io.Source
import scala.util.matching.Regex

object InputParser {

  def parseInput(fileName:String): Map[Bag, Set[Bag]] = {
    val BagRegex: Regex = "([a-z]+\\s[a-z]+) bags contain ([\\w\\s,]*)\\.".r
    val ChildBagRegex: Regex = "([0-9])+\\s([a-z]+\\s[a-z]+) bags?".r

    val source: Source = Source.fromResource(fileName)

    val bags: Map[Bag, Set[Bag]] = source.getLines().flatMap {
      _ match {
        case BagRegex(bagcolor, restOfLine) => {
          val childBags: Set[Bag] = restOfLine
            .split(",\\s")
            .flatMap(_ match {
              case ChildBagRegex(count, color) => Some(Bag(color, count.toInt))
              case _ => None
            }).toSet
          Some(Bag(bagcolor,1) -> childBags)
        }
        case _ => None
      }
    }.toMap


    source.close()

    bags
  }
}
