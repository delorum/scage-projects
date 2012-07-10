package su.msk.dunno.runnegun

import su.msk.dunno.runnegun.Runnegun._
import net.scage.support.{State, Vec}

class Enemy {
  private val trace = tracer.addTrace(tracer.randomCoord(), new GameObject {
    val state = new State("enemy")
    def changeState(changer:GameObject, s:State) {
      s.neededKeys {
        case ("hit", true) => is_alive = false
      }
    }
  })

  private var count = randomCount(1000)
  private val action_id = action {
    if(!is_alive) remove()
    else {
      // moving
      if(steps > 0) {
        tracer.moveTrace(trace, direction)
        steps -= 1
      } else {
        steps = randomCount(100)
        direction = randomDirection
      }

      // shooting
      if(count == 0) {
        val dir = Vec(0,1)
        for {
          ang <- 0 to 360 by 20
          cur_dir = dir.rotateDeg(ang)
          start = tracer.outsideCoord(trace.location + cur_dir*15)
        } {
          new Bullet(start, cur_dir, shooter_type = "enemy")
        }
        count = randomCount(1000)
      } else count -= 1
    }
  }

  private def randomDirection = Vec(math.random.toFloat, math.random.toFloat).n
  private def randomCount(max_count:Int = 100) = (math.random*max_count).toInt
  private var direction = randomDirection
  private var steps = randomCount(100)

  private val render_id = render {
    /*if(is_alive) */drawCircle(trace.location, 10, BLACK)
    /*else /*deleteSelf()*/drawCircle(trace.location, 10, BLUE)*/
  }

  private var is_alive = true
  def isAlive = is_alive

  private def remove() {
    delOperations(action_id, render_id, clear_id)
    tracer.removeTraces(trace)
  }

  private val clear_id = clear {
    remove()
  }
}