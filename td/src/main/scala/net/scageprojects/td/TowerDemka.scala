package net.scageprojects.td

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.{ScageColor, State, Vec}
import net.scage.support.tracer3.{DefaultTrace, Trace, CoordTracer}

object TowerDemka extends ScageScreenApp("Tower Demka", 800, 600) {
  val tracer = CoordTracer.create[Trace with HaveType with Damageable](
    field_from_x = 10,
    field_to_x = 790,
    field_from_y = 110,
    field_to_y = 590,
    init_N_x = 7,
    init_N_y = 5
  )

  render {
    drawTraceGrid(tracer, DARK_GRAY)
  }

  private val PLACE_TOWER = 0
  private val PLACE_WALL = 1
  private var which_building = 0

  key(KEY_1, onKeyDown = which_building = PLACE_TOWER)
  key(KEY_2, onKeyDown = which_building = PLACE_WALL)

  leftMouse(onBtnDown = {m =>
    val p = tracer.point(m)
    if(p.x > 0 && p.x < tracer.N_x-1) {
      val traces_in_point = tracer.tracesInPoint(p)
      if(traces_in_point.isEmpty) {
        which_building match {
          case PLACE_TOWER => new Tower(p)
          case PLACE_WALL => new Wall(p)
          case _ =>
        }
      } else {
        val trace_in_point = traces_in_point.head

      }
    }
  })

  private val respawn_time = property("respawn.time", 30)
  private var count = respawn_time
  init {
    count = respawn_time
  }

  action(1000) {
    count -= 1
    if(count <= 0) {
      val start = Vec(0, (math.random*tracer.N_y).toInt)
      val end = Vec(tracer.N_x-1, start.y)
      new Enemy(start, end)
      count = respawn_time
    }
  }

  interface {
    print("Next enemy spawn in "+count, 10, 10+80, WHITE)

    print("Build Mode: "+(which_building match {
      case PLACE_TOWER => "TOWER"
      case PLACE_WALL => "WALL"
      case _ =>
    })+" (Press 1 or 2 to change)", 10, 10+40, WHITE)

    print("Hitpoints: "+Base.hp, 10, 10, WHITE)
  }
}

trait HaveHitPoints {
  def hp:Int
  protected def hp_=(new_hp:Int)
}

trait SelfHitPoints extends HaveHitPoints {
  private var _hp = 0
  def hp = _hp
  protected def hp_=(new_hp:Int) {_hp = new_hp}
}

trait SelfRemovable {
  protected def remove()
}

trait Damageable extends HaveHitPoints with SelfRemovable {
  def damage(damage_amount:Int) {
    hp -= damage_amount
    if(hp <= 0) {
      remove()
    }
  }
}

trait HaveType {
  def trace_type:Int    // 0 or 1, enemy or building
}

trait EnemyType extends HaveType {
  val trace_type = 0
}

trait BuildingType extends HaveType {
  val trace_type = 1
}

import TowerDemka._

trait SelfInsertable extends Trace with HaveType with Damageable {
  def init_point:Vec
  tracer.addTrace(tracer.pointCenter(init_point), this)
}

class Tower(val init_point:Vec) extends DefaultTrace with SelfHitPoints with BuildingType with SelfInsertable {
  hp = property("tower.hp", 30)

  private var _attack = property("tower.attack", 6)
  def attack = _attack

  val attack_speed = property("tower.attack_speed", 10f)  // один выстрел в 2 секунды

  private val action_id = action {
    val enemies = tracer.tracesInPointRange(
      init_point.ix - 1 to init_point.ix + 1,
      init_point.iy - 1 to init_point.iy + 1,
      condition = {trace => trace.trace_type == 0 && trace.hp > 0})
    if(!enemies.isEmpty) {
      val nearest_enemy = enemies.foldLeft(enemies.head) {
        case (current_nearest, next_enemy) => if(next_enemy.location.dist(location) < current_nearest.location.dist(location)) next_enemy else current_nearest
      }
      shoot(nearest_enemy)
    }
  }

  /*private var can_upgrade = false
  private var upgrade_countdown = 1.5f*60
  def upgrade() {
    if(can_upgrade) {
      _attack += _attack*0.1f
      startUpgradeCountDown()
    }
  }
  def startUpgradeCountDown() {
    can_upgrade = false
    upgrade_countdown = 1.5f*60

    new {
      def remove() {
        delOperations(upgrade_clear_id, upgrade_clear_id)
      }

      val upgrade_countdown_action_id:Int = action(1000) {
        upgrade_countdown -= 1
        if(upgrade_countdown <= 0) {
          can_upgrade = true
          remove()
        }
      }

      val upgrade_clear_id:Int = clear {
        remove()
      }
    }
  }
  startUpgradeCountDown()*/

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, YELLOW)
    /*val tower_info = "HP: "+hp+"\n"+"Attack: "+_attack+"\n"+(
      if(can_upgrade) "Click To Upgrade" else upgrade_countdown
    )*/
    printCentered(/*tower_info*/hp, location)
  }

  private var last_shoot_time = 0L
  private val shoot_timeout = 2000*(attack_speed/10f)
  private def shoot(target:Trace with HaveHitPoints with Damageable) {
    if(msecsFrom(last_shoot_time) > shoot_timeout) {
      new Bullet(location, target, _attack, YELLOW)
      last_shoot_time = msecs
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

class Wall(val init_point:Vec) extends DefaultTrace with SelfHitPoints with BuildingType with SelfInsertable {
  hp = property("wall.hp", 35)

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, BLUE)
    printCentered(hp, location)
  }

  def remove() {
    delOperations(render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

object Base extends SelfHitPoints {
  hp = property("base.hp", 45)

  init {
    hp = property("base.hp", 45)
  }

  private def base_hp = hp
  private def base_hp_=(new_base_hp:Int) {hp = new_base_hp}

  for(y <- 0 until tracer.N_y) tracer.addTrace(tracer.pointCenter(Vec(tracer.N_x-1, y)), new DefaultTrace with BuildingType with HaveHitPoints with Damageable {
    def hp = base_hp
    def hp_=(new_hp:Int) {base_hp = new_hp}

    def remove() {restart()}
  })

  render {
    drawRectCentered(tracer.pointCenter(Vec(tracer.N_x-1, tracer.N_y/2)), tracer.h_x, tracer.h_y*tracer.N_y, GREEN)
  }
}

class Bullet(start_coord:Vec, target:Trace with HaveHitPoints with Damageable, damage_amount:Int, bullet_color:ScageColor) extends SelfRemovable {
  val bullet_speed = 6f

  private var lifetime = 100
  private var coord = start_coord


  private val action_id = action(10) {
    if(target.hp <= 0) remove()
    else {
      coord += (target.location - coord).n*3*(bullet_speed/6f)
      if(coord.dist(target.location) < 3) {
        target.damage(damage_amount)
        remove()
        new FlyingWord(damage_amount, bullet_color, coord, (target.location - coord))
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

class Enemy(val init_point:Vec, val end_point:Vec) extends DefaultTrace with SelfHitPoints with EnemyType with SelfInsertable {
  hp = property("enemy.hp", 30)
  val attack = property("enemy.attack", 5)
  val attack_speed = property("enemy.attack_speed", 10f)  // один хит в 2 секунды
  val speed = property("enemy.speed", 10f)         // одна клетка в 2 секунды

  private val action_id = action(10) {
    val next_coord = location + Vec(1, 0)*(tracer.h_x/200f)*(speed/10f)
    val buildings_on_next_coord = tracer.tracesInPoint(tracer.point(next_coord + Vec(20, 0)), condition = _.trace_type == 1)
    if(!buildings_on_next_coord.isEmpty) hit(buildings_on_next_coord.head)
    else tracer.updateLocation(this, next_coord)
  }

  private var last_hit_time = 0L
  private val hit_timeout = 2000*(attack_speed/10f)
  private def hit(target:Damageable) {
    if(msecsFrom(last_hit_time) > hit_timeout) {
      target.damage(attack)
      new FlyingWord(attack, RED, location, Vec(1, 0))
      last_hit_time = msecs
    }
  }

  private val render_id = render {
    drawCircle(location, 20, RED)
    printCentered(hp, location)
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
      printCentered(message, coord, color)
    } else deleteSelf()
  }
}
