package com.github.dunnololda.scageprojects.blases.levelparts

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.Relatives._
import com.github.dunnololda.scageprojects.blases.{Blase, IntersectablePolygon}
import com.github.dunnololda.scageprojects.blases.Blases._
import scala.collection.mutable

class SpeedPolygon(val intersectableVertices: List[Vec], direction: Vec = Vec.zero) extends IntersectablePolygon {
  private val dir = direction.n * rInt(200)

  /*{
        val pew = render {
          intersectableVertices.foreach(point => {
            drawRectCentered(tracer.pointCenter(point), tracer.h_x, tracer.h_y, DARK_GRAY)
            drawFilledCircle(tracer.pointCenter(point), 3, YELLOW)
            print(point.ix+":"+point.iy, tracer.pointCenter(point))
          })
        }

        clear {
          delOperations(pew, currentOperation)
        }
  }*/

  private val speeded_blases = mutable.HashMap[Blase, Vec]()
  private val after_speed_blases = mutable.HashMap[Blase, (Vec, Vec, Long)]() // blase -> (initial_speed, polygon_speed, start_time)
  private val action_id = action {
    for {
      (blase, initial_velocity) <- speeded_blases
      if !containsCoord(blase.location)
    } {
      speeded_blases -= blase
      after_speed_blases += (blase ->(initial_velocity, blase.velocity, System.currentTimeMillis()))
    }
    for {
      (blase, (initial_velocity, polygon_velocity, start_time)) <- after_speed_blases
    } {
      val cur_time = System.currentTimeMillis() - start_time
      if (cur_time > 1000 || blase.velocity == Vec.zero) after_speed_blases -= blase
      else {
        val percentage = cur_time / 1000f
        blase.velocity = initial_velocity * percentage + polygon_velocity * (1f - percentage)
      }
    }

    forEachBlaseInside {blase =>
      if (!speeded_blases.contains(blase)) {
        if (blase.velocity * dir < 0) {
          val one = dir.n * (blase.velocity * dir.n)
          val two = blase.velocity - one
          blase.velocity = one * (-1) + two
        } else {
          speeded_blases += (blase -> blase.velocity)
          blase.velocity += dir
        }
      }
    }
  }
  
  private val avg = Vec(intersectableVertices.map(_.x).sum/intersectableVertices.length, intersectableVertices.map(_.y).sum/intersectableVertices.length)
  private val arrow = List(avg + dir.n*rInt(60),
                           avg - dir.n*rInt(60),
                           avg + dir.n*rInt(60) - dir.n.rotateDeg(30)*rInt(30),
                           avg + dir.n*rInt(60) - dir.n.rotateDeg(-30)*rInt(30))

  private val render_id = render {
    drawPolygon(intersectableVertices, rColor(BLUE))
    drawLines(arrow.head, arrow(1), arrow.head, arrow(2), arrow.head, arrow(3))
  }

  clear {
    delOperations(action_id, render_id, currentOperation)
  }
}
