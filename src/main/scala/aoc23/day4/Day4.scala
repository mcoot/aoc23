package aoc23.day4

import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class Scratchcard(id: Int, winning: List[Int], have: List[Int])

object Parsing:
  def num: Parser[Int] =
    CommonParsers.int.surroundedBy(CommonParsers.char(' ').rep0)

  def numList: Parser[List[Int]] =
    num.rep(1).map(_.toList)

  def scratchcard: Parser[Scratchcard] =
    for
      _ <- CommonParsers.string("Card ")
      id <- CommonParsers.int
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
    println(input)
    0

  override def solvePart2(input: List[Scratchcard]): Int =
    0


@main def run(): Unit = Day4.run()


@main def test(): Unit = Day4.test()