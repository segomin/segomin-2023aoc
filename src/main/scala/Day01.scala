package day01

import utils.loadInput

import scala.annotation.tailrec

@main def main: Unit =
  val part1Test = """1abc2
                    |pqr3stu8vwx
                    |a1b2c3d4e5f
                    |treb7uchet""".stripMargin

  val part2Test = """two1nine
                    |eightwothree
                    |abcone2threexyz
                    |xtwone3four
                    |4nineeightseven2
                    |zoneight234
                    |7pqrstsixteen""".stripMargin
  val input = loadInput("day01.txt")

  assert(part1(part1Test) == 142)
  assert(part1(input) == 55607)

  assert(part2(part2Test) == 281)
  assert(part2(input) == 55291)

def part1(input: String) = input.linesIterator
  .map(str => getEdgeDigits(str))
  .map { case (left, right) => left * 10 + right }
  .sum

def part2(input: String) = input.linesIterator
  .map(str => replaceDigits(str))
  .map(str => getEdgeDigits(str))
  .map { case (left, right) => left * 10 + right }
  .sum

def replaceDigits(str: String): String =
  val digitToNum = List(
    ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9"),
  )

  @tailrec
  def replaceDigitsNest(after: String, before: String): String =
    if (before.isEmpty) return after

    val s2n = digitToNum.find { case (digit, _) =>
      before.startsWith(digit)
    }

    s2n match
      case Some((_, num)) => replaceDigitsNest(after + num, before.tail)
      case None => replaceDigitsNest(after + before.head, before.tail)

  replaceDigitsNest("", str)

def getEdgeDigits(str: String): (Int, Int) =
  str.toCharArray
    .filter(c => c.isDigit)
    .foldLeft((0, 0)) { case (acc, char) =>
      acc match
        case (0, 0) => (char.asDigit, char.asDigit)
        case _ => (acc._1, char.asDigit)
    }
