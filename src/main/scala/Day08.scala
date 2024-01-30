package day08

import utils.loadInput

import scala.annotation.tailrec

@main def main: Unit =
  val testInput = """RL
                    |
                    |AAA = (BBB, CCC)
                    |BBB = (DDD, EEE)
                    |CCC = (ZZZ, GGG)
                    |DDD = (DDD, DDD)
                    |EEE = (EEE, EEE)
                    |GGG = (GGG, GGG)
                    |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val testInput2 = """LLR
                     |
                     |AAA = (BBB, BBB)
                     |BBB = (AAA, ZZZ)
                     |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val input = loadInput("day08.txt")


  assert(part1(testInput) == 2)
  assert(part1(input) == 19241)


  val testInput3 ="""LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin

  assert(part2(testInput3) == 6)
  assert(part2(input) == BigInt(9606140307013L))


@tailrec
def nextStep(step: Long, key: String, instructor: Instructor, pathMap: Map[String, (String, String)]): Long = {
  if (key == "ZZZ") return step

  instructor.getNext() match {
    case 'L' => nextStep(step + 1, pathMap(key)._1, instructor, pathMap)
    case 'R' => nextStep(step + 1, pathMap(key)._2, instructor, pathMap)
  }
}

case class Instruction(key: String, direction: Char)

@tailrec
def allIntercepts(step: Long, key: String, instructor: Instructor, pathMap: Map[String, (String, String)], intercepts: Map[Instruction, Long]): Map[Instruction, Long] = {
  val instruction = Instruction(key, instructor.getNext())

  if (intercepts.contains(instruction)) return intercepts

  val accIntercepts = key.last match {
    case 'Z' => intercepts + (instruction -> step)
    case _ => intercepts
  }

  instruction.direction match {
    case 'L' => allIntercepts(step + 1, pathMap(key)._1, instructor, pathMap, accIntercepts)
    case 'R' => allIntercepts(step + 1, pathMap(key)._2, instructor, pathMap, accIntercepts)
  }
}

@tailrec
def nextStep2(step: Long, keys: List[String], instructor: Instructor, pathMap: Map[String, (String, String)]): Long = {
  if (keys.map(s => s.last).forall(c => c == 'Z')) return step

  instructor.getNext() match {
    case 'L' => nextStep2(step + 1, keys.map(key => pathMap(key)._1), instructor, pathMap)
    case 'R' => nextStep2(step + 1, keys.map(key => pathMap(key)._2), instructor, pathMap)
  }
}

class Instructor(instructions: String) {
  private var currentIndex = 0
  val length = instructions.length
  def getNext(): Char = {
    val char = instructions.charAt(currentIndex)
    currentIndex = (currentIndex + 1) % instructions.length
    char
  }
}

def part1(input: String) = {
  val (instructions: String, pathMap: Map[String, (String, String)]) = parseInput(input)
  nextStep(0, "AAA", Instructor(instructions), pathMap)
}

def part2(input: String): BigInt = {
  val (instructions: String, pathMap: Map[String, (String, String)]) = parseInput(input)
  val starts = pathMap.keys.filter(key => key.last == 'A').toList

  val leastCommonMultiple = starts.flatMap(start => {
    allIntercepts(0, start, Instructor(instructions), pathMap, Map.empty).map {
      case (instruction, step) => BigInt(step)
    }
  }).reduce((a, b) => a * b / a.gcd(b))
  leastCommonMultiple
}

def parseInput(testInput: String): (String, Map[String, (String, String)]) = {
  val inputs = testInput.split("\n").toList.splitAt(2)
  val instructions = inputs._1.head
  val pattern = """\s*(\w+)\s*=\s*\((\w+),\s*(\w+)\)\s*""".r
  val pathMap: Map[String, (String, String)] = inputs._2.map {
    case pattern(key, left, right) => key -> (left, right)
  }.toMap
  (instructions, pathMap)
}
