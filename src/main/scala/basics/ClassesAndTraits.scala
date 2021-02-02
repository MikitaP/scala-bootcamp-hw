package basics

import scala.math.{hypot, sqrt}

object ClassesAndTraits {
  sealed trait Shape extends Located with Bounded with Movable {
    def area: Double
  }

  sealed trait Movable extends {
    def move(dx: Double, dy: Double): Movable
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

    override def area: Double = 0

    def length(point: Point): Double = hypot(point.x - x, point.y - y)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double)
      extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius

    override def move(dx: Double, dy: Double): Circle =
      Circle(centerX + dx, centerY + dy, radius)

    override def area: Double = Math.PI * radius * radius
  }

  final case class Rectangle(centerX: Double,
                             centerY: Double,
                             width: Double,
                             height: Double)
      extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - width / 2
    override def maxX: Double = centerX + width / 2
    override def minY: Double = centerY - width / 2
    override def maxY: Double = centerY + width / 2

    override def move(dx: Double, dy: Double): Movable =
      Rectangle(centerX + dx, centerY + dy, width, height)

    override def area: Double = width * height

  }

  final case class Triangle(point1: Point, point2: Point, point3: Point)
      extends Shape {
    override def move(dx: Double, dy: Double): Movable =
      Triangle(
        Point(point1.x + dx, point1.y + dy),
        Point(point2.x + dx, point2.y + dy),
        Point(point3.x + dx, point3.y + dy)
      )
    override def x: Double = ???
    override def y: Double = ???
    override def minX: Double = Math.min(point1.x, Math.min(point2.x, point3.x))
    override def maxX: Double = Math.max(point1.x, Math.max(point2.x, point3.x))
    override def minY: Double = Math.min(point1.y, Math.min(point2.y, point3.y))
    override def maxY: Double = Math.max(point1.y, Math.max(point2.y, point3.y))
    override def area: Double = {
      val a = point1.length(point2)
      val b = point2.length(point3)
      val c = point3.length(point1)
      val p = (a + b + c) / 2
      sqrt(p * (p - a) * (p - b) * (p - c))
    }
  }

  final case class Square(centerX: Double, centerY: Double, side: Double)
      extends Shape {
    override def move(dx: Double, dy: Double): Movable =
      Square(centerX + dx, centerY + dx, side)
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - side / 2
    override def maxX: Double = centerX + side / 2
    override def minY: Double = centerY - side / 2
    override def maxY: Double = centerY + side / 2
    override def area: Double = side * side
  }
}
