package day02

object PasswordInputParser {

  def parse(input: String): PasswordInput = {
    val values: Array[String] = input.split("[\\s:-]+")
    PasswordInput(values(3), values(2).charAt(0), values(0).toInt, values(1).toInt)
  }
}
