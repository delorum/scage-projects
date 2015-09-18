package com.github.dunnololda.scageprojects.blases.levelparts

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.{Blase, IntersectablePolygon}
import com.github.dunnololda.scageprojects.blases.Blases._
import com.github.dunnololda.scageprojects.blases.Relatives._

class SimpleObstacle(obstacle_vertices: Vec*) extends StaticPolygon(obstacle_vertices:_*) with IntersectablePolygon {
  val intersectableVertices = obstacle_vertices.toList
  physics.addPhysicals(this)

  private val action_id = action {
    touchingBodies.foreach {
      touching_body => {
        touching_body.getUserData match {
          case blase:Blase => blase.velocity = Vec.zero
          case _ =>
        }
      }
    }
  }

  private val control_id = leftMouse(onBtnDown = {m => forEachBlaseInside {blase => blase.burst(); blases_shot -= 1}})

  private val render_id = render {
    drawPolygon(points, rColor(WHITE))
    //print(points.head.ix+":"+points.head.iy, points.head, GREEN)
  }

  clear {
    physics.removePhysicals(this)
    delOperations(action_id, render_id, control_id, currentOperation)
  }
}
