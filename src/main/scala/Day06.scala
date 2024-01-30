package day05

import utils.loadInput

@main def main: Unit =
  val testInput = """Time:      7  15   30
                    |Distance:  9  40  200""".stripMargin

  val input = loadInput("day06.txt")
  
  assert(part1(testInput) == 288)
  assert(part1(input) == 131376)
  assert(part2(testInput) == 71503)
  assert(part2(input) == 34123437)

def part1(input: String): Int =
  val games = toGameList(input)
  games.map { case (time, record) =>
    (for (speed <- 0 until time)
      yield {
        (time - speed) * speed
      }).count(_ > record)
  }.product
def part2(input: String): Int =
  val (time, record) = toOneGame(input)
  (for (speed <- 0L until time)
    yield {
      (time - speed) * speed
    }).count(_ > record)

def toGameList(input: String): Seq[(Int, Int)] =
  input.split("\n").toList match
    case timeLine :: distanceLine :: Nil =>
      val times = timeLine.split(" +").toList.tail.map(_.toInt)
      val distances = distanceLine.split(" +").toList.tail.map(_.toInt)
      times.zip(distances)

def toOneGame(input: String): (Long, Long) =
  input.split("\n").toList match
    case timeLine :: distanceLine :: Nil =>
      val time = timeLine.split(" +").toList.tail.mkString("").toLong
      val distance = distanceLine.split(" +").toList.tail.mkString("").toLong
      (time, distance)