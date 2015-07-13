package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

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
          if (ship.isSelectedEngine(e)) f"[r${e.power/1000}%.1fkN (${e.workTimeStr})]"
          else f"[o${e.power/1000}%.1fkN (${e.workTimeStr})]"
        } else f"${e.power/1000}%.1fkN (${e.workTimeStr})"
      }).mkString(", ")
    }"
    strings(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}"

  }
  override def data: Seq[String] = strings

  override val shortDescr: String = "En"
}
