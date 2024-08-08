package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4

class FlightModeInfo(playerShip: Ship4) extends InterfaceElement {
  private val strings = Array(s"Полетный режим: N/A")

  override def _update(): Unit = {
    strings(0) = s"Полетный режим: ${playerShip.flightModeStr}"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "Fm"
}
