package net.scage.blases.levelparts

import net.scage.support.physics.objects.StaticPolygon
import net.scage.support.Vec
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Relatives._
import net.scage.blases.{IntersectablePolygon, Blase}

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
