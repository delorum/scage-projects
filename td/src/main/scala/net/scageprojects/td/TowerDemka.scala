package net.scageprojects.td

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.{ScageColor, State, Vec}
import net.scage.support.tracer3.{DefaultTrace, Trace, CoordTracer}
import collection.mutable.ArrayBuffer

object TowerDemka extends ScageScreenApp("Tower Demka", 800, 600) {
  windowTitle += " - "+app_version
  val tracer = CoordTracer.create[Trace with HaveType with HaveHitPoints with Damageable](
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

  keyNoPause(KEY_SPACE, onKeyDown = switchPause())

  leftMouse(onBtnDown = {m =>
    if(all_enemies_dead) {      // allow to build and upgrade only if no enemies alive
      val p = tracer.point(m)
      if(p.x > 0 && p.x < tracer.N_x-1 && p.y >= 0 && p.y < tracer.N_y) {
        val traces_in_point = tracer.tracesInPoint(p)
        if(traces_in_point.isEmpty) {
          which_building match {
            case PLACE_TOWER => new Tower(p)
            case PLACE_WALL => new Wall(p)
            case _ =>
          }
        } else {
          val trace = traces_in_point.head
           if(trace.isTower) trace.changeState(null, State("upgrade"))
        }
      }
    }
  })

  private val respawn_period = property("respawn.period", 30)   // seconds
  private var count = respawn_period
  private var enemy_amount = property("respawn.amount", 10)
  private var enemy_increase_period = property("respawn.increase_period", 60) // seconds
  private var enemy_increase_amount = property("respawn.increase_amount", 2)

  init {
    count = respawn_period
    enemy_amount = property("respawn.amount", 10)
    enemy_increase_amount = property("respawn.increase_amount", 2)
  }

  private var start_count = msecs
  action(1000) {
    if(msecs - start_count > enemy_increase_period*1000) {
      action(enemy_increase_period*1000) {
        enemy_amount += enemy_increase_amount
      }
      deleteSelf()
    }
  }

  private var all_enemies_dead = true
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

  interface {
    if(all_enemies_dead) print("Next "+enemy_amount+" enemies will spawn in "+count, 10, 10+80, WHITE)
    else print("Attack On!!!", 10, 10+80, WHITE)

    print("Build Mode: "+(which_building match {
      case PLACE_TOWER => "TOWER"
      case PLACE_WALL => "WALL"
      case _ =>
    })+" (Press 1 or 2 to change)", 10, 10+40, WHITE)

    print("Hitpoints: "+Base.hp.formatted("%.0f"), 10, 10, WHITE)

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
  this: Trace with HaveType with HaveHitPoints with Damageable =>
  def init_coord:Vec
  tracer.addTrace(init_coord, this)
}

class Tower(init_point:Vec) extends DefaultTrace with SelfHitPoints with TowerType with SelfRemovable with Damageable with SelfInsertable {
  hp = property("tower.hp", 30)

  private var _attack = property("tower.attack", 6f)
  def attack = _attack

  val attack_speed = property("tower.attack.speed", 10f)  // один выстрел в 2 секунды

  val attack_radius = property("tower.attack.radius", 1)

  def init_coord = tracer.pointCenter(init_point)

  private val action_id = action {
    val enemies = tracer.tracesInPointRange(
      init_point.ix - attack_radius to init_point.ix + attack_radius,
      init_point.iy - attack_radius to init_point.iy + attack_radius,
      condition = {trace => trace.isEnemy && trace.hp > 0})
    if(enemies.nonEmpty) {
      val nearest_enemy = enemies.foldLeft(enemies.head) {
        case (current_nearest, next_enemy) => if(next_enemy.location.dist(location) < current_nearest.location.dist(location)) next_enemy else current_nearest
      }
      shoot(nearest_enemy)
    }
  }

  private var can_upgrade = false
  private var upgrade_countdown = (1.5f*60).toInt
  val upgrade_percentage = property("tower.upgrade.percentage", 10f)
  override def changeState(changer:Trace, s:State) {
    super.changeState(changer, s)
    s.neededKeys {
      case ("upgrade", true) =>
        if(can_upgrade) {
          _attack += _attack*upgrade_percentage*0.01f
          startUpgradeCountDown()
        }
    }
  }
  private def startUpgradeCountDown() {
    can_upgrade = false
    upgrade_countdown = (1.5f*60).toInt

    new {
      def remove() {
        delOperations(upgrade_countdown_action_id, upgrade_clear_id)
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
  startUpgradeCountDown()

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, YELLOW)
    val tower_info = "HP: "+hp.formatted("%.0f")+"\n"+"A: "+_attack.formatted("%.1f")+"\n"+(
      if(can_upgrade) "[rUpgrade]" else upgrade_countdown
    )
    printCentered(tower_info/*hp*/, location)
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
    hp = 0
    delOperations(action_id, render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

class Wall(init_point:Vec) extends DefaultTrace with SelfHitPoints with WallType with SelfRemovable with Damageable with SelfInsertable {
  hp = property("wall.hp", 35)

  def init_coord = tracer.pointCenter(init_point)

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, BLUE)
    printCentered(hp.formatted("%.0f"), location)
  }

  def remove() {
    hp = 0
    delOperations(render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

object Base extends SelfHitPoints {
  hp = property("base.hp", 45f)

  init {
    hp = property("base.hp", 45f)
  }

  private def base_hp = hp
  private def base_hp_=(new_base_hp:Float) {hp = new_base_hp}

  for(y <- 0 until tracer.N_y) tracer.addTrace(tracer.pointCenter(Vec(tracer.N_x-1, y)), new DefaultTrace with BaseType with HaveHitPoints with SelfRemovable with Damageable {
    def hp = base_hp
    def hp_=(new_hp:Float) {base_hp = new_hp}

    def remove() {restart()}
  })

  render {
    drawRectCentered(tracer.pointCenter(Vec(tracer.N_x-1, tracer.N_y/2)), tracer.h_x, tracer.h_y*tracer.N_y, GREEN)
  }
}

class Bullet(start_coord:Vec, target:Trace with HaveHitPoints with Damageable, damage_amount:Float, bullet_color:ScageColor) extends SelfRemovable {
  val bullet_speed = 6f

  private var lifetime = 100
  private var coord = start_coord


  private val action_id = action(10) {
    if(target.hp <= 0) remove()
    else {
      coord += (target.location - coord).n*3*(bullet_speed/6f)
      if(coord.dist(target.location) < 3) {
        target.changeState(null, State("damage" -> damage_amount))
        remove()
        new FlyingWord(damage_amount.formatted("%.1f"), bullet_color, coord, (target.location - coord))
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
    drawRectCentered(Vec.zero, 5, /*7*/5, bullet_color)
  }

  def remove() {
    delOperations(action_id, render_id, clear_id)
  }

  private val clear_id = clear {
    remove()
  }
}

class Enemy(val init_coord:Vec, val end_coord:Vec) extends DefaultTrace with SelfHitPoints with EnemyType with SelfRemovable with Damageable with SelfInsertable {
  hp = property("enemy.hp", 30f)
  val attack = property("enemy.attack", 5f)
  val attack_speed = property("enemy.attack.speed", 10f)  // один хит в 2 секунды
  val speed = property("enemy.speed", 10f)         // одна клетка в 2 секунды

  val dir = (end_coord - init_coord).n

  private val action_id = action(10) {
    val next_coord = location + dir*(tracer.h_x/200f)*(speed/10f)
    val buildings_on_next_coord = tracer.tracesInPoint(tracer.point(next_coord + Vec(20, 0)), condition = _.isBuilding)
    if(buildings_on_next_coord.nonEmpty) hit(buildings_on_next_coord.head)
    else tracer.updateLocation(this, next_coord)
  }

  private var last_hit_time = 0L
  private val hit_timeout = 2000*(attack_speed/10f)
  private def hit(target:Damageable) {
    if(msecsFrom(last_hit_time) > hit_timeout) {
      target.changeState(null, State("damage" -> attack))
      new FlyingWord(attack, RED, location, Vec(1, 0))
      last_hit_time = msecs
    }
  }

  private val render_id = render {
    drawCircle(location, 20, RED)
    printCentered(hp.formatted("%.0f"), location)
  }

  def remove() {
    hp = 0
    delOperations(action_id, render_id, clear_id)
    tracer.removeTraces(this)
  }

  private val clear_id = clear {
    remove()
  }
}

class FlyingWord(message:Any, color:ScageColor, init_coord:Vec, direction:Vec) {
  private var lifetime = 100
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
