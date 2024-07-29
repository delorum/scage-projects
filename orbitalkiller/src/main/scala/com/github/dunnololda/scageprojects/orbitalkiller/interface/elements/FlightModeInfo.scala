package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceElement, OrbitalKiller}

class FlightModeInfo extends InterfaceElement {
  private val strings = Array(s"Полетный режим: ${OrbitalKiller.player_ship.flightModeStr}")

  override def _update(): Unit = {
    strings(0) = s"Полетный режим: ${OrbitalKiller.player_ship.flightModeStr}"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "Fm"
}
