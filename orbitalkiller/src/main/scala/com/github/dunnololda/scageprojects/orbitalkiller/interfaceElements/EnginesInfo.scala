package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

class EnginesInfo extends InterfaceElement {
  private val strings = Array("", "Мощность и время работы отдельных двигателей:", "")

  override protected def _update(): Unit = {
    var engines_active = false
    strings(2) = s"${
      ship.engines.map(e => {
        if (e.active) {
          engines_active = true
          if (ship.isSelectedEngine(e)) f"[r${e.index}: ${e.power/1000}%.1f кН (${e.workTimeStr})]"
          else f"[o${e.index}: ${e.power/1000}%.1f кН (${e.workTimeStr})]"
        } else f"${e.index}: ${e.power/1000}%.1f кН (${e.workTimeStr})"
      }).mkString(", ")
    }"
    strings(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}"

  }
  override def data: Seq[String] = strings

  override def color:ScageColor = {
    if(isMinimized && ship.engines.exists(_.active)) ScageColor.RED else ScageColor.YELLOW
  }

  override val shortDescr: String = "En"
}
