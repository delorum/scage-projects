package com.github.dunnololda.scageprojects.blases

import com.github.dunnololda.scage.ScageLib._
import Blases._

trait IntersectablePolygon {
  def intersectableVertices: List[Vec]  // assume all real implementations are val

  private def areLinesIntersect(a1: Vec, a2: Vec, b1: Vec, b2: Vec): Boolean = {
    val common = (a2.x - a1.x) * (b2.y - b1.y) - (a2.y - a1.y) * (b2.x - b1.x)
    common != 0 && {
      val rH = (a1.y - b1.y) * (b2.x - b1.x) - (a1.x - b1.x) * (b2.y - b1.y)
      val sH = (a1.y - b1.y) * (a2.x - a1.x) - (a1.x - b1.x) * (a2.y - a1.y)

      val r = rH / common
      val s = sH / common

      r >= 0 && r <= 1 && s >= 0 && s <= 1
    }
  }

  private def containsCoord(coord: Vec, vertices: List[Vec]): Boolean = {
    if (vertices.length < 2) false
    else {
      val a1 = coord
      val a2 = Vec(Integer.MAX_VALUE, coord.y)
      val intersections = (vertices.last :: vertices.init).zip(vertices).foldLeft(0) {
        case (result, (b1, b2)) => if (areLinesIntersect(a1, a2, b1, b2)) result + 1 else result
      }
      intersections % 2 != 0
    }
  }

  private def verticesPoints(vertices:List[Vec]) = {
    val (min_x, max_x, min_y, max_y) = vertices.map(vertice => tracer.point(vertice)).foldLeft((tracer.N_x, 0, tracer.N_y, 0)) {
      case ((current_min_x, current_max_x, current_min_y, current_max_y), vertice) =>
        val new_min_x = math.min(current_min_x, vertice.ix)
        val new_max_x = math.max(current_max_x, vertice.ix)
        val new_min_y = math.min(current_min_y, vertice.iy)
        val new_max_y = math.max(current_max_y, vertice.iy)
        (new_min_x, new_max_x, new_min_y, new_max_y)
    }

    val pew1 = for {
      i <- min_x to max_x
      j <- min_y to max_y
    } yield Vec(i, j)
    val pew2 = pew1.filter(point => {
      val coord = tracer.pointCenter(point)
      containsCoord(coord, vertices) || {
        val point_borders = List(
          (coord + Vec( tracer.h_x,  tracer.h_y),
           coord + Vec( tracer.h_x, -tracer.h_y)),

          (coord + Vec( tracer.h_x, -tracer.h_y),
           coord + Vec(-tracer.h_x, -tracer.h_y)),

          (coord + Vec(-tracer.h_x, -tracer.h_y),
           coord + Vec(-tracer.h_x,  tracer.h_y)),

          (coord + Vec(-tracer.h_x, tracer.h_y),
           coord + Vec( tracer.h_x, tracer.h_y))
        )
        val polygon_borders = (vertices ::: List(Vec.zero)).zip(Vec.zero :: vertices).tail.init
        point_borders.exists {
          case ((a1, a2)) => polygon_borders.exists {
            case ((b1, b2)) =>
              areLinesIntersect(a1, a2, b1, b2)
          }
        }
      }
    })
    //println(pew2.toSet.size == pew2.size)
    pew2
  }

  def containsCoord(coord: Vec):Boolean = containsCoord(coord, intersectableVertices)
  def blasesInside = {
    verticesPoints(intersectableVertices).map(point => tracer.tracesInPoint(point)).flatten.
      filter(trace => containsCoord(trace.location, intersectableVertices))
  }
  def forEachBlaseInside(func:Blase => Any) {
    for {
      point <- verticesPoints(intersectableVertices)
      blases = tracer.tracesInPoint(point)
      blase <- blases
      if containsCoord(blase.location, intersectableVertices)
    } func(blase)
  }
}
