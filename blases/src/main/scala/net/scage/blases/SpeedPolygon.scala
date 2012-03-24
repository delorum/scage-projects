package net.scage.blases

import net.scage.support.Vec
import collection.mutable.HashMap
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.Relatives._

class SpeedPolygon(vertices: List[Vec], direction: Vec) {
  private val dir = direction.n * rInt(270)
  private val (min_x, max_x, min_y, max_y) = vertices.map(vertice => tracer.outsidePoint(tracer.point(vertice))).foldLeft((0, 0, 0, 0)) {
    case ((current_min_x, current_max_x, current_min_y, current_max_y), vertice) =>
      val new_min_x = math.min(current_min_x, vertice.ix)
      val new_max_x = math.max(current_max_x, vertice.ix)
      val new_min_y = math.min(current_min_y, vertice.iy)
      val new_max_y = math.max(current_max_y, vertice.iy)
      (new_min_x, new_max_x, new_min_y, new_max_y)
  }

  private val speeded_blases = HashMap[Blase, Vec]()
  private val action_id = action {
    for {
      (blase, initial_velocity) <- speeded_blases
      if !containsCoord(blase.location)
    } {
      blase.velocity = initial_velocity
      speeded_blases -= blase
    }
    tracer.tracesInPointRange(min_x to max_x, min_y to max_y).filter(blase => containsCoord(blase.location)).foreach(blase => {
      if (!speeded_blases.contains(blase)) {
        speeded_blases += (blase -> blase.velocity)
        blase.velocity += dir
      }
    })
  }

  private val render_id = render {
    drawPolygon(vertices, BLUE)
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

  def remove() {
    delOperations(action_id, render_id)
  }
}
