package day07

import utils.loadInput

@main def main: Unit =
  // card and money pairs
  val testInput = """32T3K 765
                    |T55J5 684
                    |KK677 28
                    |KTJJT 220
                    |QQQJA 483""".stripMargin

  val input = loadInput("day07.txt")

  assert(part1(testInput) == 6440)
  assert(part1(input) == 252656917)
  assert(part2(testInput) == 5905)
  assert(part2(input) == 253499763)

def part1(testInput: String) = {
  val priorities = "AKQJT98765432".zipWithIndex.toMap
  val result = testInput.split("\n").flatMap { line =>
    val Array(cards, money) = line.split(" ")
    val value = money.toInt
    val sortedCards = cards.groupBy(identity).view.mapValues(_.length).toList.sorted(_._2 - _._2)
    toRule(cards, value, sortedCards)
    }
  .sorted((one, two) => one.compare(two, priorities))
    .zipWithIndex.map { (rule, idx) => (idx + 1) * rule.value }
    .sum

  result
}

def part2(testInput: String) = {
  val priorities = "AKQT98765432J".zipWithIndex.toMap
  val result = testInput.split("\n").flatMap { line =>
      val Array(cards, money) = line.split(" ")
      val value = money.toInt
      val (js, ls) = cards.groupBy(identity).view.mapValues(_.length).toList.partition(_._1 == 'J')

      val sortedCards = (js, ls) match
        case (List(), _) => ls.sorted(_._2 - _._2).reverse
        case (_, List()) => js
        case _ =>
          val others = ls.sorted(_._2 - _._2).reverse
          (others.head._1, others.head._2 + js.head._2) :: others.tail
      toRule(cards, value, sortedCards.reverse)
    }.sorted((one, two) => one.compare(two, priorities))
    .zipWithIndex.map { (rule, idx) => (idx + 1) * rule.value }
    .sum

  result
}

trait Rule:
  val order: Int
  val value: Int
  val cards: String
  def compare(other: Rule, priorities: Map[Char, Int]): Int = {
    if (this.getClass != other.getClass ) other.order - order
    else {
      val left = cards.flatMap(c => priorities.get(c))
      val right = other.cards.flatMap(c => priorities.get(c))
      right.zip(left).map(x => x._1.compareTo(x._2)).find(_ != 0).getOrElse(0)
    }
  }

case class AllSame(value: Int, cards: String) extends Rule:
  val order = 0
case class FourOf(value: Int, cards: String) extends Rule:
  val order = 1
case class FullHouse(value: Int, cards: String) extends Rule:
  val order = 2
case class ThreeOf(value: Int, cards: String) extends Rule:
  val order = 3
case class TwoPair(value: Int, cards: String) extends Rule:
  val order = 4
case class OnePair(value: Int, cards: String) extends Rule:
  val order = 5
case class HighCard(value: Int, cards: String) extends Rule:
  val order = 6

def toRule(cards: String, value: Int, sortedCards: List[(Char, Int)]): Option[Rule] = {
  sortedCards match
    case List((_, 5)) => Some(AllSame(value, cards))
    case List((_, 1), (_, 4)) => Some(FourOf(value, cards))
    case List((_, 2), (_, 3)) => Some(FullHouse(value, cards))
    case List((_, 1), (_, 1), (_, 3)) => Some(ThreeOf(value, cards))
    case List((_, 1), (_, 2), (_, 2)) => Some(TwoPair(value, cards))
    case List((_, 1), (_, 1), (_, 1), (_, 2)) => Some(OnePair(value, cards))
    case _ => Some(HighCard(value, cards))
}