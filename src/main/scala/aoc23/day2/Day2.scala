package aoc23.day2


import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

enum Color:
  case RED
  case GREEN
  case BLUE

case class CubeSet(red: Int, green: Int, blue: Int):
  def power: Int = red * green * blue

case class Game(id: Int, reveals: List[CubeSet])

object Parsing:
  def gameIdParser: Parser[Int] =
    CommonParsers.string("Game ") *> CommonParsers.int

  def colorParser: Parser[Color] =
    Parser.string("red").map(_ => Color.RED) |
    Parser.string("green").map(_ => Color.GREEN) |
    Parser.string("blue").map(_ => Color.BLUE)

  def cubeParser: Parser[(Int, Color)] =
    for
      num <- CommonParsers.int
      _ <- CommonParsers.char(' ')
      col <- colorParser
    yield
      (num, col)

  def revealParser: Parser[CubeSet] =
    CommonParsers.separated(cubeParser, CommonParsers.string(", ")).map { l =>
      val resultMap: Map[Color, Int] = l.foldLeft[Map[Color, Int]](Map[Color, Int]())({
        case (m, (num, col)) =>
          val elem = col -> num
          m + elem
      })
      CubeSet(
        resultMap.getOrElse(Color.RED, 0),
        resultMap.getOrElse(Color.GREEN, 0),
        resultMap.getOrElse(Color.BLUE, 0)
      )
    }

  def gameParser: Parser[Game] =
    for
      id <- gameIdParser
      _ <- CommonParsers.string(": ")
      reveals <- CommonParsers.separated(revealParser, CommonParsers.string("; "))
    yield
      Game(id, reveals)


  def inputParser: Parser[List[Game]] =
    CommonParsers.lineSeparated(gameParser)


object Day2 extends SolutionWithParser[List[Game], Int, Int]:
  override def dayNumber: Int = 2

  override def parser: Parser[List[Game]] = Parsing.inputParser

  override def solvePart1(input: List[Game]): Int =
    val revealMax = CubeSet(12, 13, 14)
    input.filter { g =>
      g.reveals.forall(_.red <= revealMax.red) &&
        g.reveals.forall(_.green <= revealMax.green) &&
        g.reveals.forall(_.blue <= revealMax.blue)
    }.map(_.id).sum

  override def solvePart2(input: List[Game]): Int =
    input.map { g =>
      g.reveals.foldLeft[CubeSet](CubeSet(0, 0, 0)) {
        case (curMax, CubeSet(r, g, b)) => CubeSet(
          if r > curMax.red then r else curMax.red,
          if g > curMax.green then g else curMax.green,
          if b > curMax.blue then b else curMax.blue,
        )
      }.power
    }.sum

@main def run(): Unit = Day2.run()


@main def test(): Unit = Day2.test()
