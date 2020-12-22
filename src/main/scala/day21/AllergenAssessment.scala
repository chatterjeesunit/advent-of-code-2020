package day21

import scala.annotation.tailrec
import scala.io.Source

/**
 *
  5
  2072
  mxmxvkd,sqjhc,fvjkl

 */
object AllergenAssessment {
  case class Food(ingredients: Set[String], allergens: Set[String])
  case class Allergen(ingredient: String, allergen: String)

  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day21-input01.txt")
    val input2 = parseInput("day21-input02.txt")



    println("******* Part 1 - Test ************")
    val result1 = part1(input1)
    println(result1)

    println("\n******* Part 1 - Puzzle ************")
    val result2 = part1(input2)
    println(result2)

    println("\n\n******* Part 2 - Test ************")
    val result3 = part2(input1)
    println(result3)

    println("\n\n******* Part 2 - Puzzle ************")
    val result4 = part2(input2)
    println(result4)

  }

  @tailrec
  def findMatchedIngredients(allergenMap: Map[String, Set[String]], matchedIngredients: Map[String, String]): Map[String, String] = {
    if(allergenMap.isEmpty)
      return matchedIngredients

    val (matched, unmatched) = allergenMap.partition(am => am._2.size == 1)

    val newAllergenMap = unmatched.view.mapValues(_.removedAll(matched.values.flatten)).toMap
    val newMatchIngredients = matched.view.mapValues(_.mkString("")).map(m => m._2 -> m._1).toMap ++ matchedIngredients

    findMatchedIngredients(newAllergenMap, newMatchIngredients)
  }


  private def findMatchedAllergens(foodItems: List[Food]) = {
    val allergenMap = foodItems.map(f => f.allergens.map(a => a -> f.ingredients))
      .flatten
      .groupMapReduce(_._1)(_._2)(_ intersect _)

    println(allergenMap)

    val matchedIngredients = findMatchedIngredients(allergenMap, Map[String, String]())
    matchedIngredients
  }

  def part1(foodItems: List[Food]): Int = {

    val matchedIngredients: _root_.scala.Predef.Map[_root_.scala.Predef.String, _root_.scala.Predef.String] = findMatchedAllergens(foodItems)

    println(matchedIngredients)

    foodItems.map(_.ingredients.removedAll(matchedIngredients.keySet).size).sum
  }


  def part2(foodItems: List[Food]): String = {

    val matchedIngredients: _root_.scala.Predef.Map[_root_.scala.Predef.String, _root_.scala.Predef.String] = findMatchedAllergens(foodItems)

    println(matchedIngredients)

    matchedIngredients.map(m => Allergen(m._1, m._2)).toList.sortBy(_.allergen).map(_.ingredient).mkString(",")

  }



  def parseInput(fileName: String): List[Food] = {
    val foodRegex = "^(.*)\\(contains(.*)\\)$".r
    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    println(lines)
    source.close()
    lines.map(l => l match {
      case foodRegex(ingredients, allergens) => Food(
        ingredients.trim.split("\\s").toSet,
        allergens.trim.split(",\\s").toSet)
    })
  }
}
