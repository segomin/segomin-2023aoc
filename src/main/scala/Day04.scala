package day04;

import utils.loadInput
import StringExtensions._

@main def main: Unit =
  val input = loadInput("day04.txt")
  val lines = input.getLines()

  val slopes = List((1,1), (3,1), (5,1), (7,1), (1,2))
  val trees = slopes.map(slope => countTrees(lines.toList, slope._1, slope._2))
  println(trees)
 

def countTrees(lines: List[String], right: Int, down: Int): Int = {
  val width = lines.head.length
  val height = lines.length
  var x = 0
  var y = 0
  var trees = 0
  while (y < height) {
    if (lines(y)(x) == '#') trees += 1
    x = (x + right) % width
    y += down
  }
  trees

}

object StringExtensions:
  extension (s: String)
    def getLines(): Array[String] = s.split("\n")