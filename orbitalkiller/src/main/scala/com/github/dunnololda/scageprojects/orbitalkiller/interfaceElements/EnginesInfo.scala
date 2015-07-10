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
          if (ship.isSelectedEngine(e)) s"[r${e.powerPercent} % (${e.workTimeStr})]"
          else s"[o${e.powerPercent} % (${e.workTimeStr})]"
        } else s"${e.powerPercent} % (${e.workTimeStr})"
      }).mkString(", ")
    }"
    strings(0) = s"Двигательная установка: ${if (engines_active) "[rактивирована]" else "отключена"}"

  }
  override def _data: Seq[String] = strings
}
