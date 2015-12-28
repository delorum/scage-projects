package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class ShipAndCrewStateInfo extends InterfaceElement {
  private val stringsWithAirPressure = Array(
    ship.pilotStateStr,
    ship.massStr,
    ship.shadowSideStr,
    f"Атмосферное давление: ${earth.airPressureMmHg(ship.coordOrFirstPartCoord, earth.coord)}%.2f мм рт. ст. Сопротивление воздуха: ${newtonOrKilonewton(earth.airResistance(ship.currentState, earth.currentState, 28, 0.5).norma)}"
  )
  private val stringsWithoutAirPressure = Array(
    ship.pilotStateStr,
    ship.massStr,
    ship.shadowSideStr
  )

  private var selectedStrings = stringsWithAirPressure

  override val shortDescr: String = "P"

  override def data: Seq[String] = selectedStrings

  override protected def _update(): Unit = {
    if(earth.altitude(ship.coordOrFirstPartCoord, earth.coord) < earth.air_free_altitude) {
      stringsWithAirPressure(0) = ship.pilotStateStr
      stringsWithAirPressure(1) = ship.massStr
      stringsWithAirPressure(2) = ship.shadowSideStr
      stringsWithAirPressure(3) = f"Атмосферное давление: ${earth.airPressureMmHg(ship.coordOrFirstPartCoord, earth.coord)}%.2f мм рт. ст. Сопротивление воздуха: ${newtonOrKilonewton(earth.airResistance(ship.currentState, earth.currentState, 28, 0.5).norma)}"
      selectedStrings = stringsWithAirPressure
    } else {
      stringsWithoutAirPressure(0) = ship.pilotStateStr
      stringsWithoutAirPressure(1) = ship.massStr
      stringsWithoutAirPressure(2) = ship.shadowSideStr
      selectedStrings = stringsWithoutAirPressure
    }
  }
}
