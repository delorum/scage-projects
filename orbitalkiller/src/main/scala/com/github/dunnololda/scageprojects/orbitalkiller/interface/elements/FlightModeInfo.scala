package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main

class FlightModeInfo extends InterfaceElement {
  private val strings = Array(s"Полетный режим: ${Main.player_ship.flightModeStr}")

  override def _update(): Unit = {
    strings(0) = s"Полетный режим: ${Main.player_ship.flightModeStr}"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "Fm"
}
