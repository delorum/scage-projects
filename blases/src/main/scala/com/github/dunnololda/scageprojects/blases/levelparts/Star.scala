package com.github.dunnololda.scageprojects.blases.levelparts

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.Relatives._
import com.github.dunnololda.scageprojects.blases.Blases._

class Star(coord: Vec)/* extends IntersectablePolygon */{
  val star_vertices = {
    val radius = Array(rInt(30), rInt(15))
    (for {
      i <- 0 until 7 * 2
      angle = (180f / 7) * i * math.Pi / 180f
    } yield coord + Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toList
  }

  private val action_id = action {
    val blases_touch = tracer.tracesNearCoord(coord, -1 to 1, condition = {blase => (blase.location dist coord) < rInt(30)})
    if(blases_touch.nonEmpty) {
      score_for_level += 300
      new FlyingWord(300, YELLOW, coord, blases_touch.head.velocity)
      delOperations(render_id, clear_id, currentOperation)
    }
  }

  private val render_id = render {
    drawPolygon(star_vertices, rColor(YELLOW))
  }

  private val clear_id:Int = clear {
    delOperations(action_id, render_id, currentOperation)
  }
}
