package aoc23.day5


import aoc23.common.{CommonParsers, SolutionWithParser}
import aoc23.common.ParsingExtensions.*
import cats.parse.Parser


case class MappingRange(destStart: Int, sourceStart: Int, length: Int)

case class Mapping(from: String, to: String, ranges: List[MappingRange])

case class Almanac(seeds: List[Int], mappings: List[Mapping])


object Parsing:
  private def seeds: Parser[List[Int]] =
    CommonParsers.string("seeds: ") *> CommonParsers.spaceSeparated(CommonParsers.int)

  private def mappingRange: Parser[MappingRange] =
    CommonParsers.triple(
      CommonParsers.int,
      CommonParsers.int,
      CommonParsers.int,
      CommonParsers.char(' ')
    ).map {
      case (d, s, r) => MappingRange(d, s, r)
    }

  private def mappingHeader: Parser[(String, String)] =
    for
      from <- CommonParsers.alphanum.rep(1).pString
      _ <- CommonParsers.string("-to-")
      to <- CommonParsers.alphanum.rep(1).pString
      _ <- CommonParsers.string(" map:")
    yield
      (from, to)

  private def mapping: Parser[Mapping] =
    for
      h <- mappingHeader <* CommonParsers.newLine
      ranges <- CommonParsers.lineSeparated(mappingRange)
    yield
      Mapping(h._1, h._2, ranges)


  def inputParser: Parser[Almanac] =
    for
      seeds <- seeds
      _ <- CommonParsers.blankLine
      mappings <- CommonParsers.separated(mapping, CommonParsers.blankLine)
    yield
      Almanac(seeds, mappings)

object Day5 extends SolutionWithParser[Almanac, Int, Int]:
  override def dayNumber: Int = 5

  override def parser: Parser[Almanac] = Parsing.inputParser

  override def solvePart1(input: Almanac): Int =
    println(input)
    ???

  override def solvePart2(input: Almanac): Int =
    ???


@main def run(): Unit = Day5.run()


@main def test(): Unit = Day5.test()
