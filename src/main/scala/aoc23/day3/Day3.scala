package aoc23.day3

import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Rfc5234}

sealed trait GridItem

object GridItem:
  case class Digit(digit: Int) extends GridItem

  case class Symbol(symbol: Char) extends GridItem

  case object Dot extends GridItem

case class Grid(arr: Array[Array[GridItem]]):
  override def toString = s"Grid(${arr.toList.map(_.toList)})"

object Parsing:
  def gridItemParser: Parser[GridItem] =
    Parser.char('.').map(_ => GridItem.Dot) |
      Rfc5234.digit.map(c => GridItem.Digit(c.toString.toInt)) |
      Parser.charIn('!' to '@').map(c => GridItem.Symbol(c))

  def rowParser: Parser[Array[GridItem]] =
    gridItemParser.rep(1).map(_.toList.toArray)

  def inputParser: Parser[Grid] =
    CommonParsers.lineSeparated(rowParser).map(rows => Grid(rows.toArray))

object Day3 extends SolutionWithParser[Grid, Int, Int]:
  override def dayNumber: Int = 3

  override def parser: Parser[Grid] = Parsing.inputParser

  override def solvePart1(input: Grid): Int =
    print(input)
    ???

  override def solvePart2(input: Grid): Int =
    ???


@main def run(): Unit = Day3.run()


@main def test(): Unit = Day3.test()
