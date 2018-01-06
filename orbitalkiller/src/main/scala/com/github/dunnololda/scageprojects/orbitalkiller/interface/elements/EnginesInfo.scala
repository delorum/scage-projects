package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class EnginesInfo extends InterfaceElement {
  /*private var engines_work_time_msec:Double = 0
  def addWorkTime(time_msec:Double): Unit = {
    engines_work_time_msec += time_msec
  }*/

  private val strings_when_engines_nonactive = Array("")
  private val strings_when_engines_active = Array("", "")
  private var strings = strings_when_engines_nonactive

  override protected def _update(): Unit = {
    var engines_active = false
    val engines_str = s"${
      if (player_ship.isAlive) {
        player_ship.engines.filter(e => e.active || player_ship.isSelectedEngine(e)).map(e => {
          if (player_ship.isSelectedEngine(e)) {
            if (e.active) {
              engines_active = true
              f"[r${e.name}: ${e.power / 1000}%.1f кН (${e.workTimeStr})]"
            } else {
              f"[g${e.name}: ${e.power / 1000}%.1f кН (${e.workTimeStr})]"
            }
          } else {
            if (e.active) {
              engines_active = true
              f"[o${e.name}: ${e.power / 1000}%.1f кН (${e.workTimeStr})]"
            } else {
              f"${e.name}: ${e.power / 1000}%.1f кН (${e.workTimeStr})"
            }
          }
        }).mkString(", ")
      } else {
        player_ship.engines.map(e => {
          f"${e.name}: ${e.power / 1000}%.1f кН (${e.workTimeStr})"
        }).mkString(", ")
      }
    }${val x = player_ship.engines.filter(e => e.active).map(e => e.fuelConsumptionPerSec).sum; if (x != 0) f" (${gOrKg(x)} в сек)" else ""}"
    if (engines_str.isEmpty) {
      //strings_when_engines_nonactive(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}. Общее время работы двигателей: ${timeStr(engines_work_time_msec.toLong)}"
      strings_when_engines_nonactive(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}"
      strings = strings_when_engines_nonactive
    } else {
      //strings_when_engines_active(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}. Общее время работы двигателей: ${timeStr(engines_work_time_msec.toLong)}"
      strings_when_engines_active(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}"
      strings_when_engines_active(1) = engines_str
      strings = strings_when_engines_active
    }
  }

  override def data: Seq[String] = strings

  override def color: ScageColor = {
    if (isMinimized && player_ship.engines.exists(_.active)) ScageColor.RED else ScageColor.YELLOW
  }

  override val shortDescr: String = "En"
}
