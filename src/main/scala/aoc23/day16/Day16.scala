package aoc23.day16

import scala.collection.mutable.Set as MutableSet
import aoc23.common.{CommonParsers, Point2D, SolutionWithParser}
import aoc23.day16.MirrorDir.{Backward, Forward}
import aoc23.day16.SplitterDir.Horizontal
import cats.parse.Parser


enum BeamDir(val transform: Point2D):
  case Left extends BeamDir(Point2D(-1, 0))
  case Right extends BeamDir(Point2D(1, 0))
  case Up extends BeamDir(Point2D(0, -1))
  case Down extends BeamDir(Point2D(0, 1))

case class Beam(pos: Point2D, dir: BeamDir):
  def move: Beam = Beam(pos + dir.transform, dir)

sealed trait MirrorDir(val symbol: Char):
  def bend(beam: Beam): Beam

object MirrorDir:
  case object Forward extends MirrorDir('/'):
    override def bend(beam: Beam): Beam =
      val newDir = beam.dir match
        case BeamDir.Left => BeamDir.Down
        case BeamDir.Right => BeamDir.Up
        case BeamDir.Up => BeamDir.Right
        case BeamDir.Down => BeamDir.Left
      beam.copy(dir = newDir)

  case object Backward extends MirrorDir('\\'):
    override def bend(beam: Beam): Beam =
      val newDir = beam.dir match
        case BeamDir.Left => BeamDir.Up
        case BeamDir.Right => BeamDir.Down
        case BeamDir.Up => BeamDir.Left
        case BeamDir.Down => BeamDir.Right
      beam.copy(dir = newDir)

sealed trait SplitterDir(val symbol: Char):
  def split(beam: Beam): Set[Beam]

object SplitterDir:
  case object Vertical extends SplitterDir('|'):
    override def split(beam: Beam): Set[Beam] =
      beam.dir match
        case BeamDir.Up | BeamDir.Down => Set(beam)
        case BeamDir.Left | BeamDir.Right => Set(beam.copy(dir = BeamDir.Up), beam.copy(dir = BeamDir.Down))

  case object Horizontal extends SplitterDir('-'):
    override def split(beam: Beam): Set[Beam] =
      beam.dir match
        case BeamDir.Left | BeamDir.Right => Set(beam)
        case BeamDir.Up | BeamDir.Down => Set(beam.copy(dir = BeamDir.Left), beam.copy(dir = BeamDir.Right))


sealed trait Cell(val symbol: Char)

object Cell:
  case object EmptySpace extends Cell('.')

  case class Mirror(dir: MirrorDir) extends Cell(dir.symbol)

  case class Splitter(dir: SplitterDir) extends Cell(dir.symbol)

//case class CellState(cell: Cell, energized: Boolean)

case class Contraption(layout: Array[Array[Cell]]):
  def apply(pt: Point2D): Cell = layout(pt.y.toInt)(pt.x.toInt)

  def minBound: Point2D = Point2D(0, 0)

  def maxBound: Point2D = Point2D(layout(0).length - 1, layout.length - 1)

  def inBounds(pt: Point2D) = pt.inBounds(minBound, maxBound)

  def prettyPrint: String =
    val sb = StringBuilder()
    for row <- layout do
      for cell <- row do
        sb.append(cell.symbol)
      sb.append('\n')
    sb.mkString

  def prettyPrintEnergized(seenPoints: Set[Point2D]) =
    val sb = StringBuilder()
    for (row, y) <- layout.zipWithIndex do
      for (cell, x) <- layout.zipWithIndex do
        if seenPoints.contains(Point2D(x, y)) then
          sb.append('#')
        else
          sb.append('.')
      sb.append('\n')
    sb.mkString

  private def genNextBeamsFrom(beam: Beam): Set[Beam] =
    // If the beam is outside the contraption, remove it from simulation
    if !inBounds(beam.pos) then
      return Set()

    (this(beam.pos), beam.dir) match
      // Mirrors
      case (Cell.Mirror(mirrorDir), _) => Set(mirrorDir.bend(beam).move)
      // Splitters
      case (Cell.Splitter(splitterDir), _) => splitterDir.split(beam).map(_.move)
      // If the beam is in empty space, continue it on its current path
      case (Cell.EmptySpace, _) => Set(beam.move)

  def simulate: Int =
    var beams: Set[Beam] = Set(Beam(Point2D(0, 0), BeamDir.Right))
    val seen: MutableSet[Beam] = MutableSet()
    seen.addAll(beams)
    // Continue simulating until all light either leaves the contraption
    // or gets stuck in a cycle and we stop simulating it
    while beams.nonEmpty do
      val newBeams: MutableSet[Beam] = MutableSet()
      // Simulate the next states of all beams
      // If two beams 'merge' we can treat them as one (hence set)
      for beam <- beams do
        newBeams.addAll(genNextBeamsFrom(beam))
      // Drop any beams outside the grid
      newBeams.filterInPlace(b => inBounds(b.pos))
      // If we've seen a beam before (i.e. same pos and direction)
      // we can stop simulating it because it will lead to a cycle
      newBeams --= seen
      // Add all beams to our seen
      seen.addAll(newBeams)
      beams = newBeams.toSet
    // Return the number of energized cells, i.e. distinct locations a beam has been
    seen.map(_.pos).toSet.size


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
    input.simulate

  override def solvePart2(input: Contraption): Int =
    0


@main def run(): Unit = Day16.run()


@main def test(): Unit = Day16.test()