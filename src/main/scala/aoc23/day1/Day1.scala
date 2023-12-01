package aoc23.day1

import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Rfc5234}

object Day1 extends SolutionWithParser[List[String], Int, Int]:
  override def dayNumber: Int = 1

  object Parsing:
    def inputParser: Parser[List[String]] =
      CommonParsers.lineSeparated(CommonParsers.alphanum.rep(1).map(_.toList.mkString))


  override def parser: Parser[List[String]] = Parsing.inputParser

  override def solvePart1(input: List[String]): Int =
    input.map { s =>
      val dig1 = s.find(_.isDigit).get
      val dig2 = s.findLast(_.isDigit).get
      s"$dig1$dig2".toInt
    }.sum

  override def solvePart2(input: List[String]): Int =
    val digitStrs = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9,
    )
    val digitAlternatesForward = digitStrs.keys.mkString("|")
    val digitAlternatesBackward = digitAlternatesForward.reverse
    val forwardsDigitRe = (s"([0-9]|$digitAlternatesForward)").r
    val backwardsDigitRe = (s"([0-9]|$digitAlternatesBackward)").r
    input.map { s =>
      val dig1Match = forwardsDigitRe.findFirstMatchIn(s).get
      val dig2Match = backwardsDigitRe.findFirstMatchIn(s.reverse).get
      val dig1 = dig1Match.group(1) match
        case m if m.head.isDigit => m.toInt
        case m => digitStrs(m)
      val dig2 = dig2Match.group(1) match
        case m if m.head.isDigit => m.toInt
        case m => digitStrs(m.reverse)
      s"$dig1$dig2".toInt
    }.sum


@main def run(): Unit = Day1.run()

@main def test(): Unit = Day1.test()

@main def testpt2(): Unit = Day1.test("pt2")