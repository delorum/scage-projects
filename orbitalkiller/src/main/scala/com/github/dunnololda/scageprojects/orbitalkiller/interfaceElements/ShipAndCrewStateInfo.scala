package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class ShipAndCrewStateInfo extends InterfaceElement {
  private val strings = Array(ship.pilotStateStr, s"Масса корабля: ${ship.mass} кг")
  override val shortDescr: String = "P"

  override def data: Seq[String] = strings

  override protected def _update(): Unit = {
    strings(0) = ship.pilotStateStr
    strings(1) = f"Масса корабля: ${ship.mass}%.1f кг. Остаток топлива: ${ship.fuelMass}%.1f кг"
  }
}
