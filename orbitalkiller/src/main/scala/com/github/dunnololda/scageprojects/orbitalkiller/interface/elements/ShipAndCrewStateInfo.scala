package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.PlanetWithAir
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils.newtonOrKilonewton

class ShipAndCrewStateInfo(playerShip: Ship4, earth: PlanetWithAir) extends InterfaceElement {
  private val stringsWithAirPressure = Array("N/A", "N/A", "N/A", "N/A")

  private val stringsWithoutAirPressure = Array("N/A", "N/A", "N/A")

  private var selectedStrings = stringsWithAirPressure

  override val shortDescr: String = "P"

  override def data: Seq[String] = selectedStrings

  override protected def _update(): Unit = {
    val ship = playerShip.thisOrActualProxyShip
    if (earth.altitude(ship.coord, earth.coord) < earth.air_free_altitude) {
      stringsWithAirPressure(0) = playerShip.pilotStateStr
      stringsWithAirPressure(1) =
        s"${ship.massStr}. ${playerShip.fuelMassStr}" // масса топлива только игрока - потому что мы не можем управлять двигателями пристыкованного корабля, так что его топлива для нас просто мертвый груз
      stringsWithAirPressure(2) = ship.shadowSideStr
      stringsWithAirPressure(3) = {
        val air_res = earth
          .airResistance(
            ship.currentState,
            earth.currentState, /*ShipsHolder.currentShipStatesExceptShip(ship.index), */ 28,
            0.5
          )
          .norma
        f"Атмосферное давление: ${earth.airPressureMmHg(ship.coord, earth.coord)}%.2f мм рт. ст. Сопротивление воздуха: ${newtonOrKilonewton(air_res)}"
      }
      selectedStrings = stringsWithAirPressure
    } else {
      stringsWithoutAirPressure(0) = playerShip.pilotStateStr
      stringsWithoutAirPressure(1) = s"${ship.massStr}. ${playerShip.fuelMassStr}"
      stringsWithoutAirPressure(2) = ship.shadowSideStr
      selectedStrings = stringsWithoutAirPressure
    }
  }
}
