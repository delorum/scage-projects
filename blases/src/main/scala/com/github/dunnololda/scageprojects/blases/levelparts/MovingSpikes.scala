package com.github.dunnololda.scageprojects.blases.levelparts

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.Blase
import com.github.dunnololda.scageprojects.blases.Relatives._
import com.github.dunnololda.scageprojects.blases.Blases._

class MovingSpikes(start_coord: Vec, end_coord:Vec) extends StaticBall(start_coord, rInt(30), true) {
  physics.addPhysical(this)

  val vertices = {
    val radius = Array(rInt(30), rInt(15))
    (for {
      i <- 0 until 7 * 2
      angle = (180f / 7) * i * math.Pi / 180f
    } yield Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toList
  }

  private val step = (end_coord - start_coord).n
  private var dir = 1
  val speed = rFloat(2f)

  private val action_id = action {
    coord += step*speed*dir
    dir match {
      case 1 =>
        if((coord dist end_coord) < 5) dir = -1
      case -1 =>
        if((coord dist start_coord) < 5) dir = 1
      case _ =>
    }

    touchingBodies.foreach {
      touching_body => {
        val user_data = touching_body.getUserData
        user_data match {
          case blase:Blase =>
            blase.burst()
            score_for_level -= 100
            new FlyingWord(-100, YELLOW, blase.location, blase.velocity)
          case _ =>
        }
      }
    }
  }

  private val render_id = render {
    drawPolygon(vertices.map(_ + coord), rColor(RED))
  }

  clear {
    physics.removePhysicals(this)
    delOperations(action_id, render_id, currentOperation)
  }
}
