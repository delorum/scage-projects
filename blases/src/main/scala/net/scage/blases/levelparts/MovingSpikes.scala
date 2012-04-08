package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.blases.Blases._
import net.scage.ScageLib._
import net.scage.blases.Blase
import net.scage.support.physics.objects.StaticBall
import net.scage.blases.Relatives._

class MovingSpikes(start_coord: Vec, end_coord:Vec, val speed:Int) extends StaticBall(start_coord, 30, true) {
  physics.addPhysical(this)

  val vertices = {
    val radius = Array(30, 15)
    (for {
      i <- 0 until 7 * 2
      angle = ((180f / 7) * i * math.Pi / 180f)
    } yield Vec(math.cos(angle) * radius(i % 2), math.sin(angle) * radius(i % 2))).toList
  }

  private val step = (end_coord - start_coord).n
  private var dir = 1

  private val action_id = action {
    coord += step*speed*dir
    dir match {
      case 1 =>
        if((coord dist end_coord) < 60) dir = -1
      case -1 =>
        if((coord dist start_coord) < 60) dir = 1
      case _ =>
    }

    touchingBodies.foreach {
      body => {
        val user_data = body.getUserData
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
