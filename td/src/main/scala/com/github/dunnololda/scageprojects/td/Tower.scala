package com.github.dunnololda.scageprojects.td

import TowerDemka._
import com.github.dunnololda.scage.ScageLib._

object Tower {
  val tower_price = property("tower.price", 7)
  val tower_upgrade_price = property("tower.upgrade.price", 10)   // or 3 ??
  val tower_upgrade_price_increase = property("tower.upgrade.price.increase", 4)
  val tower_repair_price = property("tower.repair.price", 10)
  val tower_max_hp = property("tower.hp", 25)
  val tower_upgrade_percentage = property("tower.upgrade.percentage", 20f)  // percents
  val tower_attack_initial = property("tower.attack", 5f)
  val tower_attack_speed = property("tower.attack.speed", 10f)  // один выстрел в 1 секунду
  val tower_shoot_timeout = 1000*(10f/tower_attack_speed)       // один выстрел в 1 секунду
  val tower_attack_radius = property("tower.attack.radius", 1)
}

import com.github.dunnololda.scageprojects.td.Tower._

class Tower(init_point:Vec) extends DefaultTrace with SelfHitPoints with TowerType with SelfRemovable with Damageable with SelfInsertable {
  hp =  tower_max_hp

  private var _attack = tower_attack_initial
  def attack = _attack

  def init_coord = tracer.pointCenter(init_point)

  private val action_id = action {
    val enemies =
      tracer.tracesInPointRange(
        init_point.ix - tower_attack_radius to init_point.ix + tower_attack_radius,
        init_point.iy to init_point.iy,
        condition = {trace => trace.isEnemy && trace.hp > 0}) ++
        tracer.tracesInPointRange(
          init_point.ix to init_point.ix,
          init_point.iy - tower_attack_radius to init_point.iy + tower_attack_radius,
          condition = {trace => trace.isEnemy && trace.hp > 0})
    if(enemies.nonEmpty) {
      val nearest_enemy = enemies.foldLeft(enemies.head) {
        case (current_nearest, next_enemy) => if(next_enemy.location.dist(location) < current_nearest.location.dist(location)) next_enemy else current_nearest
      }
      shoot(nearest_enemy)
    }
  }

  private var upgrade_number = 0
  override def changeState(changer:Trace, s:State) {
    super.changeState(changer, s)
    s.neededKeys {
      case ("mouse_clicked", mouse_coord:Vec) =>
        val y_offset = location.y - mouse_coord.y
        if(y_offset >= 9 && y_offset < 24) { // Upgrade
          if(firstWaveStarted && allEnemiesDead) {   // allow to upgrade only if no enemies alive
            val upgrade_price = tower_upgrade_price + tower_upgrade_price_increase*upgrade_number
            if(resource >= upgrade_price) {
              _attack += _attack*tower_upgrade_percentage*0.01f
              hp = tower_max_hp
              upgrade_number += 1
              callEvent("Tower Upgraded", upgrade_price)
            }
          }
        } else if(y_offset >= 24 && y_offset < 37) {  // repair
          val repair_price = tower_repair_price + (tower_max_hp - hp.toInt)/2
          if(resource >= repair_price) {
            hp = tower_max_hp
            callEvent("Building Repaired", repair_price)
          }
        }
    }
  }

  private val render_id = render {
    drawRectCentered(location, tracer.h_x, tracer.h_y, YELLOW)
    /*val tower_info = "HP: "+hp.formatted("%.0f")+"\n"+"A: "+_attack.formatted("%.1f")+"\n"+ {
      val upgrade_price = tower_upgrade_price + tower_upgrade_price_increase*upgrade_number
      if(firstWaveStarted) {
        if(allEnemiesDead) {
          if(resource >= upgrade_price) "[rUpgrade ("+upgrade_price+")]" else "Upgrade ("+upgrade_price+")"
        } else ""
      } else ""
    }/*"[rUpgrade]"*/+"\n"+ {
      if(hp < tower_max_hp) {
        val repair_price = tower_repair_price + (tower_max_hp-hp.toInt)/2
        if(resource >= repair_price) "[rRepair ("+repair_price+")]" else "Repair ("+repair_price+")"
      } else ""
    }/*"[rRepair]"*/*/
    //small_font.print(tower_info, location + Vec(-35, 19))
    printCentered(hp.formatted("%.0f"), location)
  }

  private var last_shoot_time = 0L
  private def shoot(target:Trace with HaveHitPoints) {
    if(msecsFrom(last_shoot_time) > tower_shoot_timeout) {
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
