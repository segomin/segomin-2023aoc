package day03

import utils.loadInput

@main def main: Unit =
  val testInput = """467..114..
                    |...*......
                    |..35..633.
                    |......#...
                    |617*......
                    |.....+.58.
                    |..592.....
                    |......755.
                    |...$.*....
                    |.664.598..
                    |""".stripMargin

  val input = loadInput("day03.txt")

  assert(part1(testInput) == 4361)
  val result = part1(input)
  assert(part1(input) == 539590)

  assert(part2(testInput) == 467835)
  assert(part2(input) == 80703636)


class Number(val value: String, val headPoint: Point):
  override def toString: String = value + s"($headPoint)"

  def isAttached(otherPointSet: Set[Point]): Boolean = {
    toOutlinePointsOf(this).exists { (point, _) => otherPointSet.contains(point) }
  }

object Number:
  def apply(value: String, startAt: Int, rowNo: Int): Number = new Number(value, Point(startAt, rowNo))

class NumberPoint(val num: String, val point: Point):
  override def toString: String = s"$num(${point.x}, ${point.y})"

class Point(val x: Int, val y: Int):
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  //  def distance(other: Point): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
  override def hashCode(): Int = x.hashCode() + y.hashCode()

  override def equals(obj: Any): Boolean = obj match
    case other: Point => x == other.x && y == other.y
    case _ => false

  override def toString: String = s"($x, $y)"

def part1(input: String): Int = {
  val symbolPosSet: Set[Point] = getSymbolPoints(input).toSet

  input.split("\n")
    .zipWithIndex
    .flatMap((row, rowNo) => extractNumbers(row, rowNo))
    .filter(number => number.isAttached(symbolPosSet))
    .map { number => number.value.toInt }
    .sum
}

def part2(input: String): Int = {
  val board: Map[Point, Array[Number]] = input.split("\n")
    .zipWithIndex
    .flatMap((row, rowNo) => extractNumbers(row, rowNo))
    .flatMap { number => toOutlinePointsOf(number) }
    .groupBy { case (point, _) => point }
    .map { case (point, numbers) => point -> numbers.map { case (_, number) => number } }

  val symbolPositions: Array[Point] = getSymbolPoints(input)

  symbolPositions
    .flatMap ( (pt2: Point) => board.get(pt2) )
    .map { numbers =>
      numbers.toList match
        case (one :: another :: Nil) => one.value.toInt * another.value.toInt
        case _ => 0
    }.sum
}

def getSymbolPoints(input: String): Array[Point] = {
  input.split("\n")
    .zipWithIndex
    .flatMap { (row, rowNo) =>
      row.zipWithIndex
        .filter { (char, _) => isSymbol(char) }
        .map { (_, x) => Point(x, rowNo) }
    }
}
def isSymbol(char: Char) = {
  val notSymbol = '.'
  char != notSymbol && !char.isDigit
}

def extractNumbers(line: String, rowNo: Int): Seq[Number] = {
  val result: Seq[Number] = line.zipWithIndex.foldLeft(List.empty[Number] -> "") {
    case ((acc: Seq[Number], currentNum), (char, index)) =>
      if (char.isDigit) {
        (acc, currentNum + char)
      } else if (currentNum.nonEmpty) {
        (acc :+ Number(currentNum, index - currentNum.length, rowNo), "")
      } else {
        (acc, "")
      }
  } match {
    case (acc, currentNum) =>
      if (currentNum.nonEmpty) acc :+ Number(currentNum, line.length - currentNum.length, rowNo)
      else acc
  }
  result
}

def toOutlinePointsOf(number: Number): Seq[(Point, Number)] = {
  val headPoint = number.headPoint
  val tailPoint = headPoint + Point(number.value.length - 1, 0)
  val leftPoint = headPoint + Point(-1, 0)
  val rightPoint = tailPoint + Point(1, 0)
  (for
    x <- headPoint.x -1  to tailPoint.x + 1
    y <- List(headPoint.y - 1, tailPoint.y + 1)
  yield (Point(x, y), number))
    ++ List((leftPoint, number), (rightPoint, number))
}