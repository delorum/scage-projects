package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.blases.Blases._
import net.scage.ScageLib._

class Star(coord: Vec, num_beams: Int = 5, radius1: Int = 60, radius2: Int = 30) {
  private val vertices = {
    val radius = Array(radius1, radius2)
    (for {
      i <- 0 until num_beams * 2
      angle = ((180f / num_beams) * i * math.Pi / 180f)
    } yield coord + Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toList
  }
  private val vertices_zipped = if (vertices.length >= 2) {
    val vertices_shift = vertices.last :: vertices.init
    vertices_shift.zip(vertices)
  } else List[(Vec, Vec)]()

  def containsCoord(coord: Vec): Boolean = {
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

  private val action_id = action {
    val blases_touch = tracer.tracesNearCoord(coord, -1 to 1).filter(blase => containsCoord(blase.location))
    if(!blases_touch.isEmpty) {
      score_for_level += 300
      new FlyingWord(300, YELLOW, coord, blases_touch.head.velocity)
      delOperations(render_id, clear_id)
      deleteSelf()
    }
  }

  private val render_id = render {
    drawPolygon(vertices, YELLOW)
  }

  private val clear_id:Int = clear {
    delOperations(action_id, render_id)
    deleteSelf()
  }
}
