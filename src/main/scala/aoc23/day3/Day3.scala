package aoc23.day3

import scala.collection.mutable.Map as MutableMap
import aoc23.common.{CommonParsers, Point2D, SolutionWithParser}
import cats.parse.{Parser, Rfc5234}

sealed trait Item

object Item:
  // Used to represent either a single digit in initial grid
  // Or the full number in the adjacency
  case class Number(value: Int) extends Item

  case class Symbol(symbol: Char) extends Item

  case object Dot extends Item

case class Grid(arr: Array[Array[Item]]):
  def apply(pt: Point2D): Item = arr(pt.y)(pt.x)

  def bound: Point2D = Point2D(arr.length, arr(0).length)
  def maxPtIdx: Point2D = bound - Point2D(1, 1)

  override def toString = s"Grid(${arr.toList.map(_.toList)})"

  def neighbours(pt: Point2D): List[(Point2D, Item)] =
    pt.neighbourPoints.filter(_.inBounds(Point2D(0, 0), maxPtIdx)).map(pt => (pt, this(pt)))

  // Combine digits together and make an adjacency list
  // Note I'm not properly parsing number-to-number adjacencies... we'll see if that is needed for pt2
  def toAdjacency: Adjacency =
    val result: MutableMap[(Int, Item.Number), List[Item]] = MutableMap()
    var curAdj: List[Item] = List()
    // Sigh, we can have duplicates of a number so yuck
    var curAdjIdx = 0
    var curNumber: Option[Int] = None

    for y <- 0 until bound.y do
      for x <- 0 until bound.x do
        (this(Point2D(x, y)), curNumber) match
          // Starting to read a number
          case (Item.Number(value), None) =>
            // Reset the adjacency list to whatever surrounds us now other than a following digit
            curAdj = this.neighbours(Point2D(x, y))
              .filter {
                // Ignore the item to the right from adjacency if it's another number
                case (pt, Item.Number(v)) if pt == Point2D(x, y) + Point2D(1, 0) => false
                case _ => true
              }
              .map(_._2)
            curNumber = Some(value)
            // Check if we're at the end of the line as we should break here
            if x == maxPtIdx.x then
              result((curAdjIdx, Item.Number(curNumber.get))) = curAdj
              curAdj = List()
              curNumber = None
              curAdjIdx += 1
          // Continuing reading a number
          case (Item.Number(value), Some(n)) =>
            curAdj = curAdj ++ this.neighbours(Point2D(x, y))
              .filter {
                // Ignore items that we should have already included when parsing the prior digit, i.e. anything not to our right
                case (pt, _) if pt.x <= x => false
                // Ignore the item to the right from adjacency if it's another number
                case (pt, Item.Number(_)) if pt == Point2D(x, y) + Point2D(1, 0) => false
                case _ => true
              }
              .map(_._2)
            curNumber = Some(n * 10 + value)
            // Check if we're at the end of the line as we should break here
            if x == maxPtIdx.x then
              result((curAdjIdx, Item.Number(curNumber.get))) = curAdj
              curAdj = List()
              curNumber = None
              curAdjIdx += 1
          // Ending reading a number due to a non-digit item
          case (v, Some(n)) =>
            // All adjacencies should already have been captured when reading the last digit
            // So here we just save what we have and reset
            result((curAdjIdx, Item.Number(curNumber.get))) = curAdj
            curAdj = List()
            curNumber = None
            curAdjIdx += 1
          // Continue case, not currently parsing a number
          case _ =>

    Adjacency(result.toMap)


case class Adjacency(data: Map[(Int, Item.Number), List[Item]]):
  def mkString: String =
    data.map { (k, v) =>
      s"${k._1},${k._2.value}: ${v.mkString(", ")}"
    }.mkString("\n")


object Parsing:
  def gridItemParser: Parser[Item] =
    Parser.char('.').map(_ => Item.Dot) |
      Rfc5234.digit.map(c => Item.Number(c.toString.toInt)) |
      Parser.charIn('!' to '@').map(c => Item.Symbol(c))

  def rowParser: Parser[Array[Item]] =
    gridItemParser.rep(1).map(_.toList.toArray)

  def inputParser: Parser[Grid] =
    CommonParsers.lineSeparated(rowParser).map(rows => Grid(rows.toArray))


object Day3 extends SolutionWithParser[Grid, Int, Int]:
  override def dayNumber: Int = 3

  override def parser: Parser[Grid] = Parsing.inputParser

  override def solvePart1(input: Grid): Int =
    val adjList = input.toAdjacency
    println(adjList.mkString)
    val partNumbers = adjList.data.filter { case (k, adj) =>
      adj.collectFirst {
          case Item.Symbol(symbol) => symbol
      }.isDefined
    }.keys.toList
    println()
    println(partNumbers)
    println(adjList.data.keys.filter(i => !partNumbers.contains(i)).map(_._2.value).toList)
    partNumbers.map(_._2.value).sum

  override def solvePart2(input: Grid): Int =
    ???


@main def run(): Unit = Day3.run()


@main def test(): Unit = Day3.test()
