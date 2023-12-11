package aoc23.common

import scala.annotation.targetName

// Extension pair -> Point2D
extension (p: (Long, Long))
  def pos: Point2D = Point2D(p(0), p(1))

// 2D point, vector, or complex number type
case class Point2D(x: Long, y: Long):
  // Construct from pair
  def this(p: (Long, Long)) = this(p(0), p(1))

  // Convert to pair
  def pair: (Long, Long) = (x, y)

  def neighbourPoints: List[Point2D] =
    for
      x <- List(-1, 0, 1)
      y <- List(-1, 0, 1)
      if x != 0 || y != 0
    yield
      Point2D(this.x + x, this.y + y)

  // Vector / complex addition and subtraction
  @targetName("add")
  def +(other: Point2D): Point2D = Point2D(x + other.x, y + other.y)

  @targetName("subtract")
  def -(other: Point2D): Point2D = Point2D(x - other.x, y - other.y)

  // Scalar multiplication
  @targetName("mult")
  def *(scalar: Long): Point2D = Point2D(x * scalar, y * scalar)

  // Distance calculations
  def hdist(otherX: Long): Long = Math.abs(otherX - x)
  def hdist(other: Point2D): Long = hdist(other.x)

  def vdist(otherY: Long): Long = Math.abs(otherY - y)
  def vdist(other: Point2D): Long = vdist(other.y)

  def manhattan(other: Point2D): Long = hdist(other) + vdist(other)

  def magnitude: Double = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))

  // Check if in bounds
  def inBounds(minBound: Point2D, maxBound: Point2D): Boolean =
    val Point2D(minX, minY) = minBound
    val Point2D(maxX, maxY) = maxBound
    x >= minX && x <= maxX && y >= minY && y <= maxY

  def constrainToBounds(minBound: Point2D, maxBound: Point2D): Point2D =
    Point2D(Math.max(minBound.x, Math.min(maxBound.x, x)), Math.max(minBound.y, Math.min(maxBound.y, y)))

  // Modulo the components independently, useful for wrapping
  // named floorMod rather than % because it does sane (Math.floorMod) handling of negatives
  def floorMod(modulus: Point2D): Point2D =
    Point2D(Math.floorMod(x, modulus.x), Math.floorMod(y, modulus.y))