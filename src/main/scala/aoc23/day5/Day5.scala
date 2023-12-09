package aoc23.day5


import aoc23.common.{CommonParsers, SolutionWithParser}
import aoc23.common.ParsingExtensions.*
import cats.parse.Parser


case class MappingRange(destStart: Long, sourceStart: Long, length: Long):
  private def sourceRange = sourceStart until (sourceStart + length)
  private def destRange = destStart until (destStart + length)

  def performMapping(i: Long): Long = i match
    case i if sourceRange.contains(i) => i - sourceStart + destStart
    case i => i

case class Mapping(fromType: String, toType: String, ranges: List[MappingRange]):
  def performMapping(inputs: List[Long]): List[Long] =
    inputs.map { i =>
      ranges.map(_.performMapping(i)).collectFirst {
        case j if j != i => j
      }.getOrElse(i)
    }

case class Almanac(seeds: List[Long], mappings: Map[String, Mapping]):
  def execute: List[Long] =
    var values = seeds
    var curType = "seed"
    while curType != "location" do
      values = mappings(curType).performMapping(values)
      curType = mappings(curType).toType
    values



object Parsing:
  private def seeds: Parser[List[Long]] =
    CommonParsers.string("seeds: ") *> CommonParsers.spaceSeparated(CommonParsers.long)

  private def mappingRange: Parser[MappingRange] =
    CommonParsers.triple(
      CommonParsers.long,
      CommonParsers.long,
      CommonParsers.long,
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
      val mappingsMap: Map[String, Mapping] = mappings.foldLeft(Map[String, Mapping]()) { (m, mapping) =>
        val kv = (mapping.fromType, mapping)
        m + kv
      }
      Almanac(seeds, mappingsMap)

object Day5 extends SolutionWithParser[Almanac, Long, Long]:
  override def dayNumber: Int = 5

  override def parser: Parser[Almanac] = Parsing.inputParser

  override def solvePart1(input: Almanac): Long =
    input.execute.min

  override def solvePart2(input: Almanac): Long =
    ???


@main def run(): Unit = Day5.run()


@main def test(): Unit = Day5.test()
