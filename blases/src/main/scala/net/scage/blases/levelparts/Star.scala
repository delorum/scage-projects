package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Relatives._
import net.scage.blases.IntersectablePolygon

class Star(coord: Vec, num_beams: Int = 5, radius1: Int = 60, radius2: Int = 30) extends IntersectablePolygon {
  val intersectableVertices = {
    val radius = Array(radius1, radius2)
    (for {
      i <- 0 until num_beams * 2
      angle = ((180f / num_beams) * i * math.Pi / 180f)
    } yield coord + Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toList
  }

  private val action_id = action {
    val blases_touch = /*tracer.tracesNearCoord(coord, -1 to 1).filter(blase => containsCoord(blase.location))*/blasesInside
    if(!blases_touch.isEmpty) {
      score_for_level += 300
      new FlyingWord(300, YELLOW, coord, blases_touch.head.velocity)
      delOperations(render_id, clear_id, currentOperation)
    }
  }

  /*val pew = render {
    intersectableVertices.foreach(v => {
      drawRectCentered(tracer.pointCenter(tracer.point(v)), tracer.h_x, tracer.h_y, DARK_GRAY)
      drawFilledCircle(tracer.pointCenter(tracer.point(v)), 3, YELLOW)
      //print(point.ix+":"+point.iy, tracer.pointCenter(point))
    })
  }

  clear {
        delOperations(pew, currentOperation)
  }*/

  private val render_id = render {
    drawPolygon(intersectableVertices, rColor(YELLOW))
  }

  private val clear_id:Int = clear {
    delOperations(action_id, render_id, currentOperation)
  }
}
