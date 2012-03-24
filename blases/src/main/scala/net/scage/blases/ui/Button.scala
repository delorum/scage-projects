package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.Scage
import net.scage.handlers.Renderer
import net.scage.ScageLib._
import net.scage.blases.Relatives._

class Button(message: String, message_coord: Vec, width: Int, screen: Scage with Renderer) {
  def containsCoord(coord: Vec): Boolean = {
    val vertices = List(rVec(message_coord) + Vec(-5, 20),
      rVec(message_coord) + Vec(-5 + width, 20),
      rVec(message_coord) + Vec(-5 + width, -10),
      rVec(message_coord) + Vec(-5, -10))
    val vertices_zipped = if (vertices.length >= 2) {
      val vertices_shift = vertices.last :: vertices.init
      vertices_shift.zip(vertices)
    } else List[(Vec, Vec)]()
    def _areLinesIntersect(a: Vec, b: Vec, c: Vec, d: Vec): Boolean = {
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
    if (vertices.length < 2) false
    else {
      val a = coord
      val b = Vec(Integer.MAX_VALUE, coord.y)
      val intersections = vertices_zipped.foldLeft(0) {
        case (result, (c, d)) => if (_areLinesIntersect(a, b, c, d)) result + 1 else result
      }
      intersections % 2 != 0
    }
  }

  private val render_id = screen.render {
    //drawPolygon(vertices, BLACK)
    print(message, rVec(message_coord), BLACK)
  }

  def remove() {
    screen.delRenders(render_id)
  }
}
