package aoc23.day4

import scala.collection.mutable.Map as MutableMap
import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class Scratchcard(id: Int, winning: Set[Int], have: Set[Int]):
  def matching: Set[Int] = winning.intersect(have)

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
      winning <- numList.map(_.toSet)
      _ <- CommonParsers.string("|")
      have <- numList.map(_.toSet)
    yield
      Scratchcard(id, winning, have)

  def inputParser: Parser[List[Scratchcard]] =
    CommonParsers.lineSeparated(scratchcard)

object Day4 extends SolutionWithParser[List[Scratchcard], Int, Int]:
  override def dayNumber: Int = 4

  override def parser: Parser[List[Scratchcard]] = Parsing.inputParser

  override def solvePart1(input: List[Scratchcard]): Int =
    input.map { s =>
      if s.matching.isEmpty then
        0
      else
        Math.pow(2, s.matching.size - 1).intValue
    }.sum

  override def solvePart2(input: List[Scratchcard]): Int =
    val copies: MutableMap[Int, Int] =
      MutableMap.from(input.map(_.id).zip(List.fill(input.length)(1)))
    input.foreach { case s =>
      val cardsCopied = s.id + 1 to s.id + s.matching.size
      cardsCopied.foreach { copiedId =>
        copies(copiedId) += copies(s.id)
      }
    }
    copies.values.sum


@main def run(): Unit = Day4.run()

@main def test(): Unit = Day4.test()