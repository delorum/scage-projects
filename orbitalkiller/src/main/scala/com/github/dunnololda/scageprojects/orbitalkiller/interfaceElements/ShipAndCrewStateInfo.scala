package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scage.ScageLibD._

class ShipAndCrewStateInfo extends InterfaceElement {
  private val strings = Array(
    ship.pilotStateStr,
    s"Масса корабля: ${ship.mass} кг",
    f"Атмосферное давление: ${airPressurePascale(ship.coord.dist(earth.coord) - earth.radius)/133.3}%.2f мм рт. ст. Сопротивление воздуха: ${airResistance(ship.linearVelocity, ship.coord.dist(earth.coord) - earth.radius).norma}%.2f Н"
  )
  override val shortDescr: String = "P"

  override def data: Seq[String] = strings

  override protected def _update(): Unit = {
    strings(0) = ship.pilotStateStr
    strings(1) = f"Масса корабля: ${ship.mass}%.1f кг. Остаток топлива: ${ship.fuelMass}%.1f кг"
    strings(2) = f"Атмосферное давление: ${airPressurePascale(ship.coord.dist(earth.coord) - earth.radius)/133.3}%.2f мм рт. ст. Сопротивление воздуха: ${airResistance(ship.linearVelocity - (ship.coord - earth.coord).p*(earth.currentState.ang_vel.toRad*ship.coord.dist(earth.coord)), ship.coord.dist(earth.coord) - earth.radius).norma}%.2f Н"
  }
}
