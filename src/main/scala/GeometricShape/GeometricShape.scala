object GeometricShape {
  def findDifference(a: GeometricShape, b: GeometricShape) =
    Math.abs(a.area - b.area)
  def findDifference(a: Circle, b: Circle) = Math.abs(a.diameter - b.diameter)

  abstract class GeometricShape extends Comparable[GeometricShape] {
    def area: Double
    def circumference: Double

    def compareTo(shape: GeometricShape): Int = {
      if (this.area > shape.area) 1
      else if (this.area == shape.area) 0
      else -1
    }
  }

  class Circle(radius: Double) extends GeometricShape {
    val diameter = radius * 2
    override def area: Double = Math.PI * radius * radius
    override def circumference: Double = 2 * Math.PI * radius
  }

  abstract class Polygon extends GeometricShape {}

  abstract class RegularPolygon(val side: Double, val numberOfSides: Int)
      extends Polygon {
    override def circumference: Double = numberOfSides * side

    override def area: Double =
      (numberOfSides * side * side) / (4 * Math.tan(Math.PI / numberOfSides))
  }

  class Triangle(side1: Double, side2: Double, side3: Double) extends Polygon {
    private val s = (side1 + side2 + side3) / 2

    override def area: Double =
      Math.sqrt(s * (s - side1) * (s - side2) * (s - side3))
    override def circumference: Double = side1 + side2 + side3
  }

  class Square(side: Double) extends RegularPolygon(side, 4) {
    override def area: Double = side * side
    override def circumference: Double = 4 * side
  }

  class Rectangle(width: Double, height: Double) extends Polygon {
    override def area: Double = width * height
    override def circumference: Double = 2 * (width + height)
  }

  class Pentagon(side: Double) extends RegularPolygon(side, 5) {}

  class Hexagon(side: Double) extends RegularPolygon(side, 6) {}

  class Heptagon(side: Double) extends RegularPolygon(side, 7) {}
}
