package net.scage.blases.tests

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.Vec

case class Triangle(a:Vec, b:Vec, c:Vec)

object SierpinskiTest extends ScageScreenApp("Sierpinski", 640, 480) {
  def subTriangles(t:Triangle) = {
    val Triangle(a, b, c) = t
    val d:Vec = (a + b)/2
    val e:Vec = (b + c)/2
    val f:Vec = (a + c)/2
    List(Triangle(a, d, f), Triangle(d, b, e), Triangle(f, e, c))
  }

  def generateTriangles(triangles:List[Triangle], levels:Int):List[Triangle] = {
    if(levels == 0) triangles
    else generateTriangles(triangles.map(subTriangles(_)).flatten, levels - 1)
  }

  def equilateralTriangle(center:Vec, size:Int):Triangle = {
    val a = Vec(center.x - size*math.sqrt(3), center.y - size)
    val b = Vec(center.x, center.y + 2*size)
    val c = Vec(center.x + size*math.sqrt(3), center.y - size)
    Triangle(a, b, c)
  }

  val triangles = generateTriangles(List(equilateralTriangle(windowCenter - Vec(0, 70), 150)), 6)

  render {
    triangles.foreach {
      case Triangle(a, b, c) =>
        drawPolygon(List(a,b,c), WHITE)
    }
  }
}
