package aoc23.day16


import aoc23.common.{CommonParsers, Point2D, SolutionWithParser}
import aoc23.day16.MirrorDir.{Backward, Forward}
import aoc23.day16.SplitterDir.Horizontal
import cats.parse.Parser

enum MirrorDir:
  case Forward
  case Backward

  def symbol: Char = this match
    case MirrorDir.Forward => '/'
    case MirrorDir.Backward => '\\'

enum SplitterDir:
  case Vertical
  case Horizontal

  def symbol: Char = this match
    case SplitterDir.Vertical => '|'
    case SplitterDir.Horizontal => '-'

sealed trait Cell(val symbol: Char)

object Cell:
  case object EmptySpace extends Cell('.')

  case class Mirror(dir: MirrorDir) extends Cell(dir.symbol)

  case class Splitter(dir: SplitterDir) extends Cell(dir.symbol)


case class Contraption(arr: Array[Array[Cell]]):
  def apply(pt: Point2D): Cell = arr(pt.y.toInt)(pt.x.toInt)

  def prettyPrint: String =
    val sb = StringBuilder()
    arr.foreach { row =>
      row.foreach(c => sb.append(c.symbol))
      sb.append('\n')
    }
    sb.mkString

object Parsing:
  private def cell: Parser[Cell] =
    CommonParsers.char('.').map(_ => Cell.EmptySpace) |
      CommonParsers.char('/').map(_ => Cell.Mirror(MirrorDir.Forward)) |
      CommonParsers.char('\\').map(_ => Cell.Mirror(MirrorDir.Backward)) |
      CommonParsers.char('|').map(_ => Cell.Splitter(SplitterDir.Vertical)) |
      CommonParsers.char('-').map(_ => Cell.Splitter(SplitterDir.Horizontal))

  def inputParser: Parser[Contraption] =
    CommonParsers.grid(cell).map { g =>
      Contraption(g.map(_.toArray).toArray)
    }

object Day16 extends SolutionWithParser[Contraption, Int, Int]:
  override def dayNumber: Int = 16

  override def parser: Parser[Contraption] = Parsing.inputParser

  override def solvePart1(input: Contraption): Int =
    println(input.prettyPrint)
    ???

  override def solvePart2(input: Contraption): Int =
    ???


@main def run(): Unit = Day16.run()


@main def test(): Unit = Day16.test()