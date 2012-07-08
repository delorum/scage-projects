package su.msk.dunno.runnegun

import su.msk.dunno.runnegun.Runnegun._
import net.scage.support.Vec
import net.scage.support.State

class Bullet(start:Vec, val direction:Vec, val speed:Int = 2, val shooter_type:String) {
  private val trace = tracer.addTrace(start, new GameObject {
    val state = new State("bullet" -> shooter_type)
    def changeState(changer:GameObject, s:State) {}
  })

  val step = direction.n*speed
  private var steps = 200

  private def makeBulletAction(enemy_type:String) = {
    action {
      if(steps > 0) {
        tracer.moveTrace(trace, step)
        val targets = tracer.tracesNearCoord(trace.location, -1 to 1, other_trace => {
          other_trace.state.contains(enemy_type) &&
          other_trace.location.dist(trace.location) < bullet_radius + player_radius
        })
        if(targets.size > 0) {
          targets.foreach(_.changeState(trace, new State("hit")))
          steps = 0
        }
        steps -= 1
      } else {
        remove()
      }
    }
  }

  private val action_id = shooter_type match {
    case "player" => makeBulletAction("enemy")
    case "enemy"  => makeBulletAction("player")
    case _        => makeBulletAction("player")
  }

  private val render_id = shooter_type match {
    case "player" => render {drawFilledCircle(trace.location, bullet_radius, BLUE)}
    case "enemy" => render {drawFilledCircle(trace.location, bullet_radius, RED)}
    case _ => render {drawCircle(trace.location, bullet_radius, BLACK)}
  }

  private def remove() {
    delOperations(action_id, render_id, clear_id)
    tracer.removeTraces(trace)
  }

  private val clear_id = clear {
    remove()
  }
}