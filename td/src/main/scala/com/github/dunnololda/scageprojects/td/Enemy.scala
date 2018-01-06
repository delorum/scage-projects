package com.github.dunnololda.scageprojects.td

import com.github.dunnololda.scage.ScageLib._
import TowerDemka._

class Enemy(val init_coord:Vec, val end_coord:Vec) extends DefaultTrace with SelfHitPoints with EnemyType with SelfRemovable with SelfInsertable {
  hp = property("enemy.hp", 30f)
  val attack = property("enemy.attack", 5f)
  val attack_speed = property("enemy.attack.speed", 10f)  // один хит в 1 секунду
  val attack_radius = property("enemy.attack.radius", 0)
  val speed = property("enemy.speed", 10f)         // одна клетка в 2 секунды

  val dir = (end_coord - init_coord).n

  private val action_id = if(attack_radius > 0) {
    actionStaticPeriod(10) {
      val next_coord = location + dir*(tracer.h_x/100f)*(speed/10f)
      val buildings_in_radius = tracer.tracesInPoint(tracer.point(next_coord + Vec(20, 0) + Vec(attack_radius*tracer.h_x, 0)), condition = _.isBuilding)
      if(buildings_in_radius.nonEmpty) shoot(buildings_in_radius.head)
      else tracer.updateLocation(this, next_coord)
    }
  } else actionStaticPeriod(10) {
    val next_coord = location + dir*(tracer.h_x/200f)*(speed/10f)
    val buildings_on_next_coord = tracer.tracesInPoint(tracer.point(next_coord + Vec(20, 0)), condition = _.isBuilding)
    if(buildings_on_next_coord.nonEmpty) hit(buildings_on_next_coord.head)
    else tracer.updateLocation(this, next_coord)
  }

  private var last_hit_time = 0L
  private val hit_timeout = 1000*(10f/attack_speed)
  private def hit(target:Trace) {
    if(msecsFrom(last_hit_time) > hit_timeout) {
      target.changeState(null, State("damage" -> attack))
      new FlyingWord(attack, RED, location, Vec(1, 0))
      last_hit_time = msecs
    }
  }
  private def shoot(target:Trace with HaveHitPoints) {
    if(msecsFrom(last_hit_time) > hit_timeout) {
      new Bullet(location, target, attack, RED)
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

  override def changeState(changer:Trace, s:State) {
    super.changeState(changer, s)
    s.neededKeys {
      case ("damage", damage_amount:Float) =>
        hp -= damage_amount
        if(hp <= 0) {
          callEvent("Enemy Killed")
          remove()
        }
    }
  }
}