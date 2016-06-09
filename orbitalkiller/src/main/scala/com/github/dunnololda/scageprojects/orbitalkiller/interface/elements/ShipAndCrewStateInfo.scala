package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class ShipAndCrewStateInfo extends InterfaceElement {
  private val stringsWithAirPressure = Array(
  player_ship.pilotStateStr,
  player_ship.massStr,
  player_ship.shadowSideStr, {
    val air_res = earth.airResistance(player_ship.currentState, earth.currentState, ShipsHolder.currentShipStatesExceptShip(player_ship.index), 28, 0.5).norma
    f"Атмосферное давление: ${earth.airPressureMmHg(player_ship.coord, earth.coord)}%.2f мм рт. ст. Сопротивление воздуха: ${newtonOrKilonewton(air_res)}"
  }
  )
  private val stringsWithoutAirPressure = Array(
    player_ship.pilotStateStr,
    player_ship.massStr,
    player_ship.shadowSideStr
  )

  private var selectedStrings = stringsWithAirPressure

  override val shortDescr: String = "P"

  override def data: Seq[String] = selectedStrings

  override protected def _update(): Unit = {
    val ship = player_ship.thisOrActualProxyShip
    if (earth.altitude(ship.coord, earth.coord) < earth.air_free_altitude) {
      stringsWithAirPressure(0) = player_ship.pilotStateStr
      stringsWithAirPressure(1) = s"${ship.massStr}. ${player_ship.fuelMassStr}"  // масса топлива только игрока - потому что мы не можем управлять двигателями пристыкованного корабля, так что его топлива для нас просто мертвый груз
      stringsWithAirPressure(2) = ship.shadowSideStr
      stringsWithAirPressure(3) = {
        val air_res = earth.airResistance(ship.currentState, earth.currentState, ShipsHolder.currentShipStatesExceptShip(ship.index), 28, 0.5).norma
        f"Атмосферное давление: ${earth.airPressureMmHg(ship.coord, earth.coord)}%.2f мм рт. ст. Сопротивление воздуха: ${newtonOrKilonewton(air_res)}"
      }
      selectedStrings = stringsWithAirPressure
    } else {
      stringsWithoutAirPressure(0) = player_ship.pilotStateStr
      stringsWithoutAirPressure(1) = s"${ship.massStr}. ${player_ship.fuelMassStr}"
      stringsWithoutAirPressure(2) = ship.shadowSideStr
      selectedStrings = stringsWithoutAirPressure
    }
  }
}
