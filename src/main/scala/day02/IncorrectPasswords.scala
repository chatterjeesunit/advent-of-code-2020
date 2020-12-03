package day02

import scala.io.Source

/**
 * https://adventofcode.com/2020/day/2#part2
 */
object IncorrectPasswords {

  def main(args: Array[String]): Unit = {

    val passwordList: List[PasswordInput] =
      Source.fromResource("day02-input.txt")
        .getLines()
        .map(input => PasswordInputParser.parse(input))
        .toList

    checkPasswordsByFirstPolicy(passwordList)
    checkPasswordsBySecondPolicy(passwordList)
  }

  private def checkPasswordsByFirstPolicy(passwordList: List[PasswordInput]) = {
    val validPasswords: List[PasswordInput] = passwordList.filter(p => {
      val occurences = p.password.filter(c => c.equals(p.characterToMatch)).size
      if (occurences >= p.lowerLimit && occurences <= p.upperLimit) true else false
    })

    println(s"Valid Passwords (First Policy) = ${validPasswords.size}")
  }

  private def checkPasswordsBySecondPolicy(passwordList: List[PasswordInput]) = {
    val validPasswords: List[PasswordInput] = passwordList.filter(p => {
      val isFirstPositionMatch: Boolean = p.password(p.lowerLimit - 1).equals(p.characterToMatch)
      val isSecondPositionMatch: Boolean = p.password(p.upperLimit - 1).equals(p.characterToMatch)
      (isFirstPositionMatch, isSecondPositionMatch) match {
        case (true, false) => true
        case (false, true) => true
        case _ => false
      }
    })

    println(s"Valid Passwords (Second Policy) = ${validPasswords.size}")
  }
}
