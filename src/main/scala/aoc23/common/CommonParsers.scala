package aoc23.common

import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser, Parser0, Rfc5234}

object ParsingExtensions:
  extension[T] (p: Parser[NonEmptyList[T]])
    def pList: Parser[List[T]] = p.map(_.toList)

  extension (p: Parser[NonEmptyList[Char]])
    def pString: Parser[String] = p.pList.map(_.mkString)

object CommonParsers:
  val spaces: Parser[Any] = Parser.char(' ').rep

  val newLine: Parser0[Any] = Parser.char('\r').? ~ Parser.char('\n')

  val spaceOrNewline: Parser0[Any] = spaces | newLine

  val blankLine: Parser0[Any] = newLine ~ newLine

  val int: Parser[Int] = Numbers.signedIntString.map(_.toInt)

  val long: Parser[Long] = Numbers.signedIntString.map(_.toLong)

  val alphanum: Parser[Char] = Rfc5234.alpha | Rfc5234.digit

  def char(char: Char): Parser[Char] =
    Parser.char(char).map(_ => char)

  def string(string: String): Parser[String] =
    Parser.string(string).map(_ => string)

  def pair[A, B](a: Parser[A], b: Parser[B], sep: Parser0[Any]): Parser[(A, B)] =
    (a <* sep) ~ b

  def triple[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C], sep: Parser0[Any]): Parser[(A, B, C)] =
    for
      aVal <- a
      _ <- sep
      bVal <- b
      _ <- sep
      cVal <- c
    yield
      (aVal, bVal, cVal)

  def separated[A](p: Parser[A], sep: Parser0[Any]): Parser[List[A]] =
    p.repSep(1, sep).map(_.toList)

  def commaSeparated[A](p: Parser[A]): Parser[List[A]] =
    separated(p, Parser.char(','))

  def spaceSeparated[A](p: Parser[A]): Parser[List[A]] =
    separated(p, spaces)

  def lineSeparated[A](p: Parser[A]): Parser[List[A]] =
    separated(p, newLine)

  def withTrimmedStartingSpaces[A](p: Parser[A]): Parser[A] =
    (spaces.?).with1 *> p

  def indented[A](p: Parser[A], indent: Int): Parser[A] =
    Parser.char(' ').rep(indent, indent) *> p