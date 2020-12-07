package day07

import scala.collection.mutable

/**
 * https://adventofcode.com/2020/day/7
 * Answer Puzzle 1 - Input 1 : 4
 * Answer Puzzle 1 - Input 2 : 335
 * Answer Puzzle 2 - Input 2 : 2431
 */
object ColoredBagsPuzzle {



  def main(args: Array[String]): Unit = {

    /*** PUZZLE 1 **/

    val input1: Map[Bag, Set[Bag]] = InputParser.parseInput("day07-input01.txt")
    val parentBags0: Set[Bag] = findParentBags("shiny gold", input1)
    println(s"Found ${parentBags0.size} parent bags.........${parentBags0.map(_.color).toList}")


    val input2: Map[Bag, Set[Bag]] = InputParser.parseInput("day07-input02.txt")
    val parentBags1: Set[Bag] = findParentBags("shiny gold", input2)
    println(s"Found ${parentBags1.size} parent bags.")


    /*** PUZZLE 2 **/

    val n: Int = findCountOfChildBags(Bag("shiny gold", 1), input2)
    println(s"Total ${n} bags.")
  }



  def findParentBags(colorToFind: String, bags: Map[Bag, Set[Bag]]): Set[Bag] = {

    val queue: mutable.Queue[Bag] = mutable.Queue.empty
    val visitedBags: mutable.Set[Bag] = mutable.Set.empty
    val parentBags: mutable.Set[Bag] = mutable.Set.empty

    queue.addOne(Bag(colorToFind, 0))

    while(!queue.isEmpty) {
      val bagToFind: Bag = queue.dequeue()
      if(!visitedBags.contains(bagToFind)){
        visitedBags.add(bagToFind)
        bags.map { case (parentBag, childBag) => {
          if(childBag.contains(bagToFind)) {
            queue.addOne(parentBag)
            parentBags.add(parentBag)

          }
        }}
      }
    }
    parentBags.toSet
  }


  def findCountOfChildBags(bagToFind: Bag, input: Map[Bag, Set[Bag]]): Int = {
    input.get(bagToFind) match {
      case Some(childBags) => childBags.map(b => b.count + b.count * findCountOfChildBags(b, input)).sum
      case _ => 0
    }
  }

}
