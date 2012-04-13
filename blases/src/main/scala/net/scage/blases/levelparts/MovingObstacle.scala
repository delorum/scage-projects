package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.blases.{Blase, IntersectablePolygon}
import net.scage.blases.Blases._
import net.scage.blases.Relatives._
import net.scage.ScageLib._
import net.phys2d.raw.{FixedAngleJoint, BasicJoint, FixedJoint}
import net.scage.support.physics.objects.{DynaBox, StaticBox, StaticPolygon}
import collection.mutable.{HashMap, ArrayBuffer}

class MovingObstacle(obstacle_vertices:List[Vec], start_coord: Vec, end_coord:Vec) extends StaticPolygon(obstacle_vertices:_*) {
  physics.addPhysicals(this)

  private val step = (end_coord - start_coord).n
  private var dir = 1
  val speed = rFloat(2f)
  private val joined_blases = HashMap[Blase, Vec]()

  private val action_id = action {
    coord += step*speed*dir
    dir match {
      case 1 =>
        if((coord dist end_coord) < 5) dir = -1
      case -1 =>
        if((coord dist start_coord) < 5) dir = 1
      case _ =>
    }

    for {
      (blase, blase_coord) <- joined_blases
    } {
      if(blase.bursted) joined_blases -= blase
      else blase.coord = coord + blase_coord
    }

    touchingBodies.foreach {
      touching_body => {
        touching_body.getUserData match {
          case blase:Blase if(!joined_blases.contains(blase)) =>
            blase.velocity = Vec.zero
            joined_blases += (blase -> (blase.coord - coord))
          case _ =>
        }
      }
    }
  }

  private val render_id = render {
    drawPolygon(points, rColor(WHITE))
    //print(points.head.ix+":"+points.head.iy, points.head, GREEN)
  }

  clear {
    physics.removePhysicals(this)
    delOperations(action_id, render_id, currentOperation)
  }
}
