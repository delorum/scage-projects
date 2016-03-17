package com.github.dunnololda.scageprojects.orbitalkiller.interaface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

/*class SatelliteEscapeVelocityInfo extends InterfaceElement {
  private val strings = Array("")
  override protected def _update(): Unit = {
    insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val sat_speed = satelliteSpeed(ship.coord, ship.linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G).norma
        val esc_speed = escapeVelocity(ship.coord, ship.linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G).norma
        val ship_speed = ship.linearVelocity.norma
        if(ship_speed < sat_speed) {
          strings(0) = f"Скорость корабля/спутника/убегания: ${planet.name} [c${msecOrKmsec(ship_speed)}]/${msecOrKmsec(sat_speed)}/${msecOrKmsec(esc_speed)}"
        } else if(ship_speed < esc_speed) {
          strings(0) = f"Скорость спутника/корабля/убегания: ${planet.name} ${msecOrKmsec(sat_speed)}/[c${msecOrKmsec(ship_speed)}]/${msecOrKmsec(esc_speed)}"
        } else {
          strings(0) = f"Скорость спутника/убегания/корабля: ${planet.name} ${msecOrKmsec(sat_speed)}/${msecOrKmsec(esc_speed)}/[c${msecOrKmsec(ship_speed)}]"
        }
      case None =>
        strings(0) = ""
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "SEV"
}*/
