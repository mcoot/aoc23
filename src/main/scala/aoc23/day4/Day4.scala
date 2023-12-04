package aoc23.day4

import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class Scratchcard(id: Int, winning: List[Int], have: List[Int])

object Parsing:
  private def num: Parser[Int] =
    CommonParsers.int.surroundedBy(CommonParsers.char(' ').rep0)

  private def numList: Parser[List[Int]] =
    num.rep(1).map(_.toList)

  private def scratchcard: Parser[Scratchcard] =
    for
      _ <- CommonParsers.string("Card ")
      id <- num
      _ <- CommonParsers.string(":")
      winning <- numList
      _ <- CommonParsers.string("|")
      have <- numList
    yield
      Scratchcard(id, winning, have)

  def inputParser: Parser[List[Scratchcard]] =
    CommonParsers.lineSeparated(scratchcard)

object Day4 extends SolutionWithParser[List[Scratchcard], Int, Int]:
  override def dayNumber: Int = 4

  override def parser: Parser[List[Scratchcard]] = Parsing.inputParser

  override def solvePart1(input: List[Scratchcard]): Int =
    input.map { s =>
      val haveWinning = s.winning.toSet.intersect(s.have.toSet)
      if haveWinning.isEmpty then
        0
      else
        Math.pow(2, haveWinning.size - 1).intValue
    }.sum

  override def solvePart2(input: List[Scratchcard]): Int =
    0


@main def run(): Unit = Day4.run()


@main def test(): Unit = Day4.test()