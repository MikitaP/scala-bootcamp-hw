package basics

import scala.math.{hypot, sqrt}

object ClassesAndTraits {

  // 2D shapes
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

  // 3D shapes
  sealed trait Shape3D
      extends Located3D
      with Movable3D
      with SurfaceArea
      with Volume
      with Bounded3D

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait SurfaceArea {
    def surfaceArea: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  final case object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def volume: Double = 0
    override def surfaceArea: Double = 0

    override def move(dx: Double, dy: Double, dz: Double): Shape3D =
      Point3D(dx + x, dy + y, dz + z)

    def length(point: Point): Double = hypot(point.x - x, point.y - y)
  }

  final case class Sphere(centerX: Double,
                          centerY: Double,
                          centerZ: Double,
                          radius: Double)
      extends Shape3D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def minZ: Double = centerZ - radius
    override def maxZ: Double = centerZ + radius

    override def volume: Double = 4 / 3 * Math.PI * math.pow(radius, 3)
    override def surfaceArea: Double = 4 * Math.PI * math.pow(radius, 2)

    override def move(dx: Double, dy: Double, dz: Double): Shape3D =
      Sphere(dx + x, dy + y, dz + z, radius)
  }

  final case class Cube(centerX: Double,
                        centerY: Double,
                        centerZ: Double,
                        rib: Double)
      extends Shape3D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = centerX - rib / 2
    override def maxX: Double = centerX + rib / 2
    override def minY: Double = centerY - rib / 2
    override def maxY: Double = centerY + rib / 2
    override def minZ: Double = centerZ - rib / 2
    override def maxZ: Double = centerZ + rib / 2

    override def volume: Double = math.pow(rib, 3)
    override def surfaceArea: Double = 6 * math.pow(rib, 2)

    override def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(x + dx, y + dy, z + dz, rib)
  }

  final case class Cuboid(x: Double,
                          y: Double,
                          z: Double,
                          width: Double,
                          height: Double,
                          length: Double)
      extends Shape3D {

    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height
    override def minZ: Double = z
    override def maxZ: Double = z + length

    override def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(x + dx, y + dy, z + dz, width, height, length)

    override def volume: Double = length * width * height
    override def surfaceArea: Double =
      2 * (length * width + length * height + width * height)
  }

  final case class Triangle3D(point1: Point3D,
                              point2: Point3D,
                              point3: Point3D,
                              point4: Point3D)
      extends Shape3D {
    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???

    override def minX: Double =
      List(point1, point2, point3, point4).map(_.x).min
    override def maxX: Double =
      List(point1, point2, point3, point4).map(_.x).max
    override def minY: Double =
      List(point1, point2, point3, point4).map(_.y).min
    override def maxY: Double =
      List(point1, point2, point3, point4).map(_.y).max
    override def minZ: Double =
      List(point1, point2, point3, point4).map(_.z).min
    override def maxZ: Double =
      List(point1, point2, point3, point4).map(_.z).max

    override def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(
        Point3D(point1.x + dx, point1.y + dy, point1.z + dz),
        Point3D(point2.x + dx, point2.y + dy, point2.z + dz),
        Point3D(point3.x + dx, point3.y + dy, point3.z + dz),
        Point3D(point4.x + dx, point4.y + dy, point4.z + dz)
      )

    override def volume: Double = ???
    override def surfaceArea: Double = ???
  }
}
