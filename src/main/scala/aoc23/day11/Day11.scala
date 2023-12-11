package aoc23.day11

import scala.collection.mutable.Set as MutableSet
import aoc23.common.{CommonParsers, Point2D, SolutionWithParser}
import aoc23.common.ParsingExtensions.*
import cats.parse.Parser


case class ImageGrid(arr: Array[Array[Boolean]]):
  def toImage: Image =
    val s: MutableSet[Point2D] = MutableSet()
    for (r, y) <- arr.zipWithIndex do
      for (cell, x) <- r.zipWithIndex do
        if cell then
          s.add(Point2D(x, y))
    Image(s.toSet)

  def mkString: String =
    val sb = StringBuilder()
    for r <- arr do
      for cell <- r do
        sb.append(if cell then '#' else '.')
      sb.append('\n')
    sb.toString()

case class Image(galaxies: Set[Point2D]):
  private def extent: Point2D =
    val maxX = galaxies.map(_.x).maxOption.getOrElse(0L)
    val maxY = galaxies.map(_.y).maxOption.getOrElse(0L)
    Point2D(maxX, maxY)

  def expand(n: Int): Image =
    val newPoints: MutableSet[Point2D] = MutableSet()
    val maxPt = this.extent
    val xValsToExpand: Set[Long] = (0L to maxPt.x).toSet.diff(galaxies.map(_.x))
    val yValsToExpand: Set[Long] = (0L to maxPt.y).toSet.diff(galaxies.map(_.y))

    for pt <- galaxies do
      newPoints.add(
        Point2D(
          pt.x + xValsToExpand.count(_ < pt.x) * (n - 1),
          pt.y + yValsToExpand.count(_ < pt.y) * (n - 1)
        )
      )

    Image(newPoints.toSet)

  def shortestPathLengths: List[(Point2D, Point2D, Long)] =
    galaxies.toList.flatMap { g1 =>
      galaxies.toList.filter(_ != g1).map { g2 =>
        (g1, g2, g1.manhattan(g2))
      }
    }

  def toGrid: ImageGrid =
    val maxPt = this.extent
    val arr: Array[Array[Boolean]] = Array.fill(maxPt.y.toInt+1)(Array.fill(maxPt.x.toInt+1)(false))
    for y <- 0 to maxPt.y.toInt do
      for x <- 0 to maxPt.x.toInt do
        arr(y)(x) = galaxies.contains(Point2D(x, y))
    ImageGrid(arr)


object Parsing:
  private def pointPresent: Parser[Boolean] =
    CommonParsers.char('.').map(_ => false) |
      CommonParsers.char('#').map(_ => true)

  private def row: Parser[Array[Boolean]] =
    pointPresent.rep(1).pList.map(_.toArray)

  def inputParser: Parser[ImageGrid] =
    CommonParsers.lineSeparated(row).map(v => ImageGrid(v.toArray))


object Day11 extends SolutionWithParser[ImageGrid, Long, Long]:
  override def dayNumber: Int = 11

  override def parser: Parser[ImageGrid] = Parsing.inputParser

  override def solvePart1(input: ImageGrid): Long =
    input.toImage.expand(2).shortestPathLengths.map(_._3).sum / 2

  override def solvePart2(input: ImageGrid): Long =
    input.toImage.expand(1000000).shortestPathLengths.map(_._3).sum / 2


@main def run(): Unit = Day11.run()


@main def test(): Unit = Day11.test()

