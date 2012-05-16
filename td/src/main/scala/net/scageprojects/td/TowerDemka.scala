package net.scageprojects.td

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.tracer3.{Trace, CoordTracer}
import net.scage.support.{ScageColor, State, Vec, PathFinder}

object TowerDemka extends ScageScreenApp("Tower Demka", 800, 600) {
  val tracer = CoordTracer(
    field_from_x = 10,
    field_to_x = 790,
    field_from_y = 110,
    field_to_y = 590,
    init_N_x = 7,
    init_N_y = 5
  )

  render {
    drawTraceGrid(tracer, DARK_GRAY)
    //drawTraceLocations(tracer, GREEN, 4)
  }

  private val PLACE_TOWER = 0
  private val PLACE_WALL = 1
  private var which_building = 0

  key(KEY_1, onKeyDown = which_building = PLACE_TOWER)
  key(KEY_2, onKeyDown = which_building = PLACE_WALL)

  leftMouse(onBtnDown = {m =>
    val p = tracer.point(m)
    if(p.x > 0 && p.x < tracer.N_x-1) {
      val buildings_in_point = tracer.tracesInPoint(p, condition = trace => trace.state.contains("building"))
      if(buildings_in_point.isEmpty) {
        which_building match {
          case PLACE_TOWER => new Tower(p)
          case PLACE_WALL => new Wall(p)
          case _ =>
        }
      }
    }
  })

  private var count = 30+1
  init {
    count = 30+1
  }

  action(1000) {
    count -= 1
    if(count <= 0) {
      val start = Vec(0, (math.random*tracer.N_y).toInt)
      val end = Vec(tracer.N_x-1, start.y)
      new Enemy(start, end)
      count = 30+1
    }
  }

  Base

  interface {
    print("Next enemy spawn in "+count, 10, 10+80, WHITE)

    print("Build Mode: "+(which_building match {
      case PLACE_TOWER => "TOWER"
      case PLACE_WALL => "WALL"
      case _ =>
    })+" (Press 1 or 2 to change)", 10, 10+40, WHITE)

    print("Health: "+Base.hp, 10, 10, WHITE)
  }
}

import TowerDemka._

class Tower(point:Vec) extends Trace {
  var hp = 30
  var attack = 6
  var attack_speed = 10f  // один выстрел в 2 секунды

  tracer.addTrace(tracer.pointCenter(point), this)

  private val action_id = action {
    val enemies = tracer.tracesInPointRange(
      point.ix - 1 to point.ix + 1,
      point.iy - 1 to point.iy + 1,
      condition = {trace => trace.state.contains("enemy") && trace.state.value[Int]("hp") > 0})
    if(!enemies.isEmpty) {
      val nearest_enemy = enemies.foldLeft(enemies.head) {
        case (current_nearest, next_enemy) => if(next_enemy.location.dist(location) < current_nearest.location.dist(location)) next_enemy else current_nearest
      }
      shoot(nearest_enemy)
    }
  }

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, YELLOW)
    printCentered(hp, location)
  }

  private var last_shoot_time = 0L
  private var shoot_timeout = 2000*(attack_speed/10f)
  private def shoot(target:Trace) {
    if(msecsFrom(last_shoot_time) > shoot_timeout) {
      new Bullet(location, target, attack, YELLOW)
      last_shoot_time = msecs
    }
  }

  def state = State("building" -> true, "tower" -> true, "hp" -> hp)
  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("damage", damage:Int) =>
        hp -= damage
        if(hp <= 0) {
          remove()
        }
    }
  }

  def remove() {
    delOperations(action_id, render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

class Wall(point:Vec) extends Trace {
  var hp = 35

  tracer.addTrace(tracer.pointCenter(point), this)

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, BLUE)
    printCentered(hp, location)
  }

  def state = State("building" -> true, "wall" -> true, "hp" -> hp)
  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("damage", damage:Int) =>
        hp -= damage
        if(hp <= 0) {
          remove()
        }
    }
  }

  def remove() {
    delOperations(render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

object Base {
  var hp = 45
  init {
    hp = 45
  }

  for(y <- 0 until tracer.N_y) tracer.addTrace(tracer.pointCenter(Vec(tracer.N_x-1, y)), new Trace {
    def state = State("building" -> true, "base" -> true, "hp" -> hp)
    def changeState(changer:Trace, s:State) {
      s.neededKeys {
        case ("damage", damage:Int) =>
          hp -= damage
          if(hp <= 0) {
            restart()
          }
      }
    }
  })

  render {
    drawRectCentered(tracer.pointCenter(Vec(tracer.N_x-1, tracer.N_y/2)), tracer.h_x, tracer.h_y*tracer.N_y, GREEN)
  }
}

class Bullet(start_coord:Vec, target:Trace, damage:Int, bullet_color:ScageColor) {
  val bullet_speed = 6f

  private var lifetime = 100
  private var coord = start_coord


  private val action_id = action(10) {
    if(target.state.value[Int]("hp") <= 0) remove()
    else {
      coord += (target.location - coord).n*3*(bullet_speed/6f)
      if(coord.dist(target.location) < 3) {
        target.changeState(null, State("damage" -> damage))
        remove()
        new FlyingWord(damage, bullet_color, coord, (target.location - coord))
      } else {
        lifetime -= 1
        if(lifetime <= 0) {
          remove()
        }
      }
    }
  }

  private val render_id = render {
    openglMove(coord)
    openglRotateDeg((target.location - coord).deg(Vec(0,1)))  // TODO: fix rotation!
    drawRectCentered(Vec.zero, 5, 7, bullet_color)
  }

  def remove() {
    delOperations(action_id, render_id, clear_id)
  }

  private val clear_id = clear {
    remove()
  }
}

class Enemy(init_point:Vec, end_point:Vec) extends Trace {
  var hp = 30
  val attack = 5
  val attack_speed = 10f  // один хит в 2 секунды
  val speed = 10f         // одна клетка в 2 секунды

  tracer.addTrace(tracer.pointCenter(init_point), this)

  private val action_id = action(10) {
    val next_coord = location + Vec(1, 0)*(tracer.h_x/200f)*(speed/10f)
    val traces_on_next_coord = tracer.tracesInPoint(tracer.point(next_coord + Vec(20, 0)))
    traces_on_next_coord.find(trace => trace.state.contains("building")) match {
      case Some(building) => hit(building)
      case None => tracer.updateLocation(this, next_coord)
    }
  }

  private var last_hit_time = 0L
  private val hit_timeout = 2000*(attack_speed/10f)
  private def hit(target:Trace) {
    if(msecsFrom(last_hit_time) > hit_timeout) {
      target.changeState(this, State("damage" -> attack))
      new FlyingWord(attack, RED, location, Vec(1, 0))
      last_hit_time = msecs
    }
  }

  private val render_id = render {
    drawCircle(location, 20, RED)
    printCentered(hp, location)
  }

  def state = State("enemy" -> true, "hp" -> hp)
  def changeState(changer:Trace, s:State) {
    s.neededKeys {
      case ("damage", damage:Int) =>
        hp -= damage
        if(hp <= 0) {
          remove()
        }
    }
  }

  def remove() {
    delOperations(action_id, render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) {
  private var lifetime = 100;
  private val dir = direction.n
  private var coord = init_coord

  action(10) {
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
