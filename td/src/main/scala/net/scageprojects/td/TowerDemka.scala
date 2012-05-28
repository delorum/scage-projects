package net.scageprojects.td

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.{ScageColor, State, Vec}
import net.scage.support.tracer3.{DefaultTrace, Trace, CoordTracer}
import collection.mutable.ArrayBuffer
import net.scage.support.messages.ScageMessage
import Tower._
import Wall._

object TowerDemka extends ScageScreenApp("Tower Demka", 800, 600) {
  windowTitle += " - "+app_version
  val tracer = CoordTracer.create[Trace with HaveType with HaveHitPoints with SelfRemovable](
    field_from_x = 10,
    field_to_x = 790,
    field_from_y = 150,
    field_to_y = 590,
    init_N_x = 15,
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

  keyNoPause(KEY_SPACE, onKeyDown = switchPause())

  leftMouse(onBtnDown = {m =>
    val p = tracer.point(m)
    if(p.x > 0 && p.x < tracer.N_x-1 && p.y >= 0 && p.y < tracer.N_y) {
      val traces_in_point = tracer.tracesInPoint(p)
      if(traces_in_point.isEmpty) {
        which_building match {
          case PLACE_TOWER =>
            if(_resource >= tower_price) {
              _resource -= tower_price
              new Tower(p)
            }
          case PLACE_WALL =>
            if(_resource >= wall_price) {
              _resource -= wall_price
              new Wall(p)
            }
          case _ =>
        }
      } else {
        val trace = traces_in_point.head
        if(trace.isBuilding) {
          trace.changeState(null, State("mouse_clicked" -> m))
        }
      }
    }
  })

  private val respawn_period = property("respawn.period", 30)   // seconds
  private var count = respawn_period
  private var enemy_amount = property("respawn.amount", 10)

  init {
    count = respawn_period
    enemy_amount = property("respawn.amount", 10)
    enemy_increase_amount = property("respawn.increase_amount", 2)
  }

  private var enemy_first_increase_period = property("respawn.first_increase_period", 15) // seconds
  private var enemy_increase_period = property("respawn.increase_period", 20) // seconds
  private var enemy_increase_amount = property("respawn.increase_amount", 2)
  private var start_count = msecs
  action(1000) {
    if(msecs - start_count > enemy_first_increase_period*1000) {
      action(enemy_increase_period*1000) {
        enemy_amount += enemy_increase_amount
      }
      deleteSelf()
    }
  }

  private var all_enemies_dead = true
  def areAllEnemiesDead = all_enemies_dead
  def spawnEnemies() {
    all_enemies_dead = false
    val enemies = ArrayBuffer[Enemy]()
    action(500) {
      if(enemies.length < enemy_amount) {
        val start_point = Vec(0, (math.random*tracer.N_y).toInt)
        if(tracer.tracesInPoint(start_point).isEmpty) {
          val start = tracer.pointCenter(start_point)
          val end = Vec((tracer.N_x-1)*tracer.h_x + tracer.h_x/2, start.y)
          enemies += new Enemy(start, end)
        }
      } else {
        deleteSelf()
        action(1000) {
          all_enemies_dead = enemies.forall(_.hp <= 0)
          if(all_enemies_dead) {
            nextWaveCountdown()
            deleteSelf()
          }
        }
      }
    }
  }

  def nextWaveCountdown() {
    action(1000) {
      count -= 1
      if(count <= 0) {
        spawnEnemies()
        count = respawn_period
        deleteSelf()
      }
    }
  }
  nextWaveCountdown()

  private var _resource = property("resource.initial_amount", 80)
  def resource = _resource
  init {
    _resource = property("resource.initial_amount", 80)
  }

  val resource_from_enemy = property("resource.from_enemy", 5)
  onEvent("Enemy Killed") {
    _resource += resource_from_enemy
  }

  interface {
    if(all_enemies_dead) print("Next "+enemy_amount+" enemies will spawn in "+count, 10, 10+120, WHITE)
    else print("Attack On!!!", 10, 10+120, WHITE)

    print("Build Mode: "+(which_building match {
      case PLACE_TOWER => "TOWER"
      case PLACE_WALL => "WALL"
      case _ =>
    })+" (Press 1 or 2 to change)", 10, 10+80, WHITE)

    print("Hitpoints: "+Base.hp.formatted("%.0f"), 10, 10+40, WHITE)

    print("Resource: "+_resource, 10, 10, WHITE)

    if(onPause) printCentered("PAUSE (Press Space)", windowWidth/2, 20, WHITE)
  }
}

trait HaveHitPoints {
  def hp:Float
  protected def hp_=(new_hp:Float)
}

trait SelfHitPoints extends HaveHitPoints {
  private var _hp = 0f
  def hp = _hp
  protected def hp_=(new_hp:Float) {_hp = new_hp}
}

trait SelfRemovable {
  protected def remove()
}

trait Damageable extends Trace {
  this: HaveHitPoints with SelfRemovable =>
  abstract override def changeState(changer:Trace, s:State) {
    super.changeState(changer, s)
    s.neededKeys {
      case ("damage", damage_amount:Float) =>
        hp -= damage_amount
        if(hp <= 0) {
          remove()
        }
    }
  }
}

trait HaveType {
  def isEnemy:Boolean
  def isBuilding:Boolean
  def isTower:Boolean
  def isWall:Boolean
  def isBase:Boolean
}

trait EnemyType extends HaveType {
  def isEnemy    = true
  def isBuilding = false
  def isTower    = false
  def isWall     = false
  def isBase     = false
}

trait BuildingType extends HaveType {
  def isEnemy    = false
  def isBuilding = true
}

trait TowerType extends BuildingType {
  def isTower    = true
  def isWall     = false
  def isBase     = false
}

trait WallType extends BuildingType {
  def isTower    = false
  def isWall     = true
  def isBase     = false
}

trait BaseType extends BuildingType {
  def isTower    = false
  def isWall     = false
  def isBase     = true
}

import TowerDemka._

trait SelfInsertable {
  this: Trace with HaveType with HaveHitPoints with SelfRemovable =>
  def init_coord:Vec
  tracer.addTrace(init_coord, this)
}