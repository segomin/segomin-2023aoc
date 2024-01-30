package day02

import utils.loadInput


@main def main: Unit =
  val testInput = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                    |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                    |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                    |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                    |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  val input = loadInput("day02.txt")

  assert(part1(testInput) == 8)
  assert(part1(input) == 2331)
  assert(part2(input) == 71585)

  val result = Round("0 red, 1 green, 2 blue").max(Round("2 red, 1 green, 0 blue"))
  assert(result == Round("2 red, 1 green, 2 blue"))

def part2(input: String) =
  input.split("\n")
    .map(Game(_))
    .map(game => {
      val rounds = game.rounds
      val maxRound: Round = rounds.foldLeft(Round.zero) { (acc, round) =>
        acc.max(round)
      }
      maxRound.productCount()
    })
    .sum

def part1(input: String) =
  val budget = Round("12 red, 13 green, 14 blue")
  input.split("\n")
    .map(Game(_))
    .filter(game => game.isPlayable(budget))
    .map(_.num)
    .sum

case class Round(red: Int, green: Int, blue: Int) {
  def isPlayable(budget: Round): Boolean =
    red <= budget.red && green <= budget.green && blue <= budget.blue

  def max(other: Round): Round =
    Round(red.max(other.red), green.max(other.green), blue.max(other.blue))

  def productCount(): Int = red * green * blue
}

case class Game(num: Int, rounds: List[Round]) {
  def isPlayable(budget: Round): Boolean =
    rounds.forall(round => round.isPlayable(budget))
}

object Game:
  def apply(str: String): Game =
    val gameAndRounds = str.split(":")

    val gameIdx = gameAndRounds(0).split(" ")(1).toInt
    val rounds = gameAndRounds(1).split(";").map(Round(_)).toList
    Game(gameIdx, rounds)

object Round:
  def zero: Round = Round("0 red, 0 green, 0 blue")

  def apply(str: String): Round =
    val cubes = str.split(",").map(_.trim).map(Cube(_))
    val colors = cubes.map(cube => (cube.color, cube)).toMap
    Round(colors.getOrElse("red", Cube("red", 0)).num,
          colors.getOrElse("green", Cube("green", 0)).num,
          colors.getOrElse("blue", Cube("blue", 0)).num)

case class Cube(color: String, num: Int)

object Cube:
  def apply(str: String): Cube =
    val color = str.split(" ")(1)
    val num = str.split(" ")(0).toInt
    Cube(color, num)
