package day04

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

/**
 * https://adventofcode.com/2020/day/4
 */
object PassportProblem {

  case class FV(field: String, regex: Regex) {
    def validate(data: String): Boolean = {
      data match {
        case regex(_*) => true
        case _ => false
      }
    }
  }

  case class PassportValidator(fieldValidators: List[FV], desc: String) {
    def validate(data: Passport): Boolean = {
      fieldValidators.forall(v => {
        data.fields.get(v.field) match {
          case Some(value) => v.validate(value)
          case _ => false
        }
      })
    }
  }

  case class Passport(fields: Map[String, String])


  def main(args: Array[String]): Unit = {

    val passportValidators1 = PassportValidator(
      List(
        FV("ecl", ".*".r), FV("pid", ".*".r), FV("eyr", ".*".r), FV("hcl", ".*".r),
        FV("byr", ".*".r), FV("iyr", ".*".r), FV("hgt", ".*".r)
      ),
      "Passport Field Validators only"
    )

    val passportValidators2 = PassportValidator(
      List(
        FV("ecl", "(amb|blu|brn|gry|grn|hzl|oth)".r),   FV("pid", "([0-9]{9})".r),
        FV("eyr", "(202[0-9]|2030)".r),                 FV("hcl", "(#[0-9a-f]{6})".r),
        FV("byr", "(19[2-9][0-9]|200[0-2])".r),         FV("iyr", "(201[0-9]|2020)".r),
        FV("hgt", "(1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in)".r)
      ),
      "Passport Field and Data Format Validators"
    )

    getValidPassports("day04-input01.txt", passportValidators1)
    getValidPassports("day04-input02.txt", passportValidators1)

    getValidPassports("day04-input01.txt", passportValidators2)
    getValidPassports("day04-input02.txt", passportValidators2)
  }

  def getValidPassports(fileName: String, validator: PassportValidator): Unit = {
    val source: BufferedSource = Source.fromResource(fileName)
    val passportData: List[Passport] = readPassportData(source)
    val validPassports: List[Passport] = passportData.filter(p => validator.validate(p))
    println(s"File = $fileName, Validation = ${validator.desc}. # of valid Passports = ${validPassports.size}")
    source.close()
  }

  private def readPassportData(source: BufferedSource): List[Passport]= {
    source.getLines()
      .mkString("|")
      .split("\\|{2,}").toList
      .map(str =>
        Passport(
          str.split("[\\|\\s]")
            .map(s => {
              val arr = s.split(":")
              (arr(0), arr(1))
            })
          .toMap))
  }
}
