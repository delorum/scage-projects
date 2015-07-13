package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class SatelliteEscapeVelocityInfo extends InterfaceElement {
  private val strings = Array("")
  override protected def _update(): Unit = {
    insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val planet_name = planet.index match {
          case earth.index => "Земля"
          case moon.index => "Луна"
        }
        val sat_speed = satelliteSpeed(ship.coord, ship.linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G).norma
        val esc_speed = escapeVelocity(ship.coord, ship.linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G).norma
        strings(0) = f"Скорость спутника/убегания: $planet_name ${msecOrKmsec(sat_speed)}/${msecOrKmsec(esc_speed)}"
      case None =>
        strings(0) = ""
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "SEV"
}
