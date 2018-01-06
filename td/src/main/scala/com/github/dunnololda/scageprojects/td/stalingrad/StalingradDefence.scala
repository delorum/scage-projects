package com.github.dunnololda.scageprojects.td.stalingrad

import com.github.dunnololda.scage.ScageLib._
import org.newdawn.slick.util.pathfinding.Mover

import scala.collection.mutable.ArrayBuffer

object StalingradDefence extends ScageScreenApp("Stalingrad Defence", 800, 600) {
  val tracer = CoordTracer(
    field_from_x = 10,
    field_to_x = 790,
    field_from_y = 110,
    field_to_y = 590,
    init_h_x = 20,
    init_h_y = 20
  )
  val path_finder = PathFinder(tracer.N_x, tracer.N_y, is_blocked = (x, y) => tracer.tracesInPoint(x, y).exists(trace => trace.state.contains("impassable")))

  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp())

  render {
    drawTraceGrid(tracer, DARK_GRAY)
    drawTraceLocations(tracer, GREEN, 4)
  }

  leftMouse(onBtnDown = {m =>
    val p = tracer.point(m)
    if(p.x < tracer.N_x-1 && p.y > 1 && p.y < tracer.N_y-1) {
      val towers_nearby = tracer.tracesInPointRange(p.ix - 1 to p.ix + 2,
                                                    p.iy - 2 to p.iy + 1,
                                                    condition = {trace => trace.state.contains("tower")})
      if(towers_nearby.isEmpty) new Tower(p)
    }
  })

  private var all_nazis_dead = true
  def wave(amount:Int) {
    val enemies = for(i <- 0 until amount) yield {
      val start = Vec((math.random*tracer.N_x).toInt, tracer.N_y - 1)
      val end = Vec((math.random*tracer.N_x).toInt, 0)
      new Enemy(start, end)
    }

    actionStaticPeriod(1000) {
      all_nazis_dead = enemies.forall(_.hp <= 0)
      if(all_nazis_dead) {
        spawnNextWaveIn20Sec()
        deleteSelf()
      }
    }
  }

  private var count = 20+1
  def spawnNextWaveIn20Sec() {
    actionStaticPeriod(1000) {
      count -= 1
      if(count <= 0) {
        wave(50)
        count = 20+1
        deleteSelf()
      }
    }
  }
  spawnNextWaveIn20Sec()

  private val corpses = ArrayBuffer[Vec]()
  private val corpses_limit = 200
  def addCorpse(corpse_coord:Vec) {
    corpses += corpse_coord
    if(corpses.size > corpses_limit) corpses --= corpses.take(40)
  }
  render(2) {
    for(corpse_coord <- corpses) drawCircle(corpse_coord, 10, BLUE)
  }

  private var enemies_reached_stalingrad = 0
  onEvent("Stalingrad Reached") {enemies_reached_stalingrad += 1}

  private var enemies_shot_down = 0
  onEvent("Enemy Down") {enemies_shot_down += 1}

  interface {
    if(all_nazis_dead) print("Nazis next attack in "+count, 10, 10+80, WHITE)
    else print("Attack On!!!", 10, 10+80, WHITE)

    print("Nazis Down: "+enemies_shot_down, 10, 10+40, WHITE)
    print("Nazis Reached Stalingrad: "+enemies_reached_stalingrad, 10, 10, WHITE)
  }
}

import StalingradDefence._

class Tower(upper_left_point:Vec) {
  private def towerPart = new Trace {
    def state = State("tower" -> true, "impassable" -> true)
    def changeState(changer:Trace, s:State) {}
  }

  val tower_traces = tracer.addTraces((tracer.pointCenter(upper_left_point),              towerPart),
                                      (tracer.pointCenter(upper_left_point + Vec(1, 0)),  towerPart),
                                      (tracer.pointCenter(upper_left_point + Vec(0, -1)), towerPart),
                                      (tracer.pointCenter(upper_left_point + Vec(1, -1)), towerPart))
  
  val tower_center = tracer.pointCenter(upper_left_point) + Vec(tracer.h_x/2, -tracer.h_y/2)

  private var last_shoot_time = 0L
  private var shoot_timeout = 200  // msec
  private def shoot(target:Trace) {
    if(msecsFrom(last_shoot_time) > shoot_timeout) {
      new Bullet(tower_center, target)
      last_shoot_time = msecs
    }
  }

  action {
    val enemies = tracer.tracesInPointRange(
      upper_left_point.ix - 2 to upper_left_point.ix + 3,
      upper_left_point.iy - 3 to upper_left_point.iy + 2,
      condition = {trace => trace.state.contains("enemy") && trace.state.value[Int]("hp").exists(_ > 0)})
    if(enemies.nonEmpty) {
      val nearest_enemy = enemies.foldLeft(enemies.head) {
        case (current_nearest, next_enemy) => if(next_enemy.location.dist(tower_center) < current_nearest.location.dist(tower_center)) next_enemy else current_nearest
      }
      shoot(nearest_enemy)
    }
  }
  
  render(1) {
    drawRectCentered(tower_center, tracer.h_x*2, tracer.h_y*2, WHITE)
  }

  callEvent("New Tower Placed")
}

class Enemy(init_point:Vec, end_point:Vec) extends Trace with Mover {
  private var _hp = 100
  def hp = _hp

  tracer.addTrace(tracer.pointCenter(init_point), this)

  private var path = path_finder.findPath(init_point, end_point).map(tracer.pointCenter)
  private var next_coord = tracer.pointCenter(init_point)
  private val event_id = onEvent("New Tower Placed") {
    path = path_finder.findPath(tracer.point(location), end_point).map(tracer.pointCenter)
    if(path.nonEmpty) next_coord = path.pop()
  }

  private val action_id = actionStaticPeriod(10) {
    if((location dist next_coord) < 3) {
      if(path.nonEmpty) next_coord = path.pop()
      else {
        // do some damage to the player
        callEvent("Stalingrad Reached")
        _hp = 0
        remove()
      }
    } else tracer.updateLocation(this, location + (next_coord - location).n)
  }

  private val render_id = render {
    drawCircle(location, 10, WHITE)
    drawFilledCircle(next_coord, 3, RED)
  }

  def state = State("enemy" -> true, "hp" -> _hp)
  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("damage", damage:Int) =>
        _hp -= damage
        if(_hp <= 0) {
          remove()
          addCorpse(location)
          callEvent("Enemy Down")
        }
    }
  }

  def remove() {
    delOperations(action_id, render_id)
    tracer.removeTraces(this)
    delEvents(event_id)
  }
}

class Bullet(start_coord:Vec, target:Trace) {
  val speed = 3

  private var lifetime = 50
  private var coord = start_coord


  actionStaticPeriod(10) {
    if(target.state.value[Int]("hp").exists(_ <= 0)) delOperations(render_id, currentOperation)
    else {
      coord += (target.location - coord).n*speed
      if(coord.dist(target.location) < 3) {
        val damage = (math.random*20).toInt
        target.changeState(null, State("damage" -> damage))
        delOperations(render_id, currentOperation)
        new FlyingWord(damage, RED, coord, target.location - coord)
      } else {
        lifetime -= 1
        if(lifetime <= 0) {
          delOperations(render_id, currentOperation)
        }
      }
    }
  }

  private val render_id = render {
    openglMove(coord)
    openglRotate(-(target.location - coord).deg(Vec(0,1)))
    drawRectCentered(Vec.zero, 5, 7, WHITE)
  }
}

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) {
  private var lifetime = 100
  private val dir = direction.n
  private var coord = init_coord

  actionStaticPeriod(10) {
    if(lifetime > 0) {
      coord += dir
      lifetime -= 1
    } else deleteSelf()
  }

  render {
    if(lifetime > 0) {
      print(message, coord, color)
    } else deleteSelf()
  }
}
