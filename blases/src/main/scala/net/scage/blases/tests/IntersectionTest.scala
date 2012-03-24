package net.scage.blases.tests

import net.scage.ScageScreenApp
import net.scage.support.Vec
import net.scage.ScageLib._

object IntersectionTest extends ScageScreenApp("Intersection Test") {
  val polygon_points = Array(Vec(415, 538), Vec(523, 621), Vec(737, 364), Vec(639, 284))

  render {
    currentColor = YELLOW
    drawPolygon(polygon_points)
  }

  def areLinesIntersect(a: Vec, b: Vec, c: Vec, d: Vec): Boolean = {
    val common = (b.x - a.x) * (d.y - c.y) - (b.y - a.y) * (d.x - c.x)
    if (common == 0) false
    else {
      val rH = (a.y - c.y) * (d.x - c.x) - (a.x - c.x) * (d.y - c.y)
      val sH = (a.y - c.y) * (b.x - a.x) - (a.x - c.x) * (b.y - a.y)

      val r = rH / common
      val s = sH / common

      if (r >= 0 && r <= 1 && s >= 0 && s <= 1) true
      else false
    }
  }

  def containsCoord(coord: Vec): Boolean = {
    val a = coord
    val b = Vec(Integer.MAX_VALUE, coord.y)
    var intersections = 0
    if (areLinesIntersect(a, b, Vec(415, 538), Vec(523, 621))) intersections += 1
    if (areLinesIntersect(a, b, Vec(523, 621), Vec(737, 364))) intersections += 1
    if (areLinesIntersect(a, b, Vec(737, 364), Vec(639, 284))) intersections += 1
    if (areLinesIntersect(a, b, Vec(639, 284), Vec(415, 538))) intersections += 1

    intersections % 2 != 0
  }

  leftMouse(onBtnDown = {
    m =>
      println(containsCoord(m))
  })
}
