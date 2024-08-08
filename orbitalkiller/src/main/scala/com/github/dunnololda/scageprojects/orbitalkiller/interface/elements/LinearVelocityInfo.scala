package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4

class LinearVelocityInfo(playerShip: Ship4) extends InterfaceElement {
  private val strings = Array(f"Линейная скорость: N/A")

  override protected def _update(): Unit = {
    strings(0) = f"Линейная скорость: ${playerShip.thisOrActualProxyShipVelocityStr}"
  }

  override def data: Seq[String] = strings

  override val color: ScageColor = ScageColor.CYAN

  override val shortDescr: String = "LV"
}
