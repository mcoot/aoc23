package aoc23.day15


import scala.collection.mutable.Map as MutableMap
import aoc23.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Rfc5234}

sealed trait Op:
  def toLiteralString: String

object Op:
  case class Equals(value: Int) extends Op:
    override def toLiteralString: String = s"=${value}"

  case object Minus extends Op:
    override def toLiteralString: String = "-"

def runHash(s: String) =
  s.toList.foldLeft(0) { (v, c) =>
    Math.floorMod((v + c.toInt) * 17, 256)
  }

case class Step(label: String, op: Op):
  def toLiteralString = s"${label}${op.toLiteralString}"

case class Lens(label: String, value: Int)

case class State(m: MutableMap[Int, List[Lens]]):
  def exec(step: Step): Unit =
    val box = runHash(step.label)
    val items = m.getOrElse(box, List())
    val idxOfLabel = items.map(_.label).indexOf(step.label)
    step.op match
      case Op.Equals(focalLength) =>
        if idxOfLabel >= 0 then
          m(box) = items.updated(idxOfLabel, Lens(step.label, focalLength))
        else
          m(box) = items.appended(Lens(step.label, focalLength))
      case Op.Minus =>
        if idxOfLabel >= 0 then
          val split = items.splitAt(idxOfLabel)
          m(box) = split._1 ++ split._2.tail

  def focusingPower: Long =
    m.map { (box, lenses) =>
      lenses.zipWithIndex.map { (l, idx) =>
        (1L + box) * (1L + idx) * l.value
      }.sum
    }.sum


object State:
  def initial: State = State(MutableMap())

object Parsing:
  private def op: Parser[Op] =
    (CommonParsers.char('=') *> CommonParsers.int).map(v => Op.Equals(v)) |
      CommonParsers.char('-').map(_ => Op.Minus)

  private def step: Parser[Step] =
    for
      label <- Rfc5234.alpha.rep(1).map(_.toList.mkString)
      opVal <- op
    yield
      Step(label, opVal)

  def inputParser: Parser[List[Step]] =
    CommonParsers.commaSeparated(step)


object Day15 extends SolutionWithParser[List[Step], Int, Long]:
  override def dayNumber: Int = 15

  override def parser: Parser[List[Step]] = Parsing.inputParser

  override def solvePart1(input: List[Step]): Int =
    input.map(s => runHash(s.toLiteralString)).sum

  override def solvePart2(input: List[Step]): Long =
    val state = State.initial
    input.foreach(step => state.exec(step))
    state.focusingPower


@main def run(): Unit = Day15.run()


@main def test(): Unit = Day15.test()