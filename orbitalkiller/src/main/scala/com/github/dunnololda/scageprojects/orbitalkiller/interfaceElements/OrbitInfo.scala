package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scage.ScageLibD._

class OrbitInfo extends InterfaceElement {
  private val strings = Array("", ""/*, ""*/)
  override protected def _update(): Unit = {
    strings(0) = s"Параметры орбиты:"
    strings(1) = orbitStrInPointWithVelocity(ship.coordOrFirstPartCoord, ship.linearVelocity, ship.mass, currentPlanetStates)

    /*val equation = insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val orbit = calculateOrbit(planet_state.mass, planet_state.coord, ship.mass, ship.coord - planet_state.coord, ship.linearVelocity - planet_state.vel, G)
        orbit match {
          case e:EllipseOrbit =>
            val coef = e.r_p*(1+e.e)
            s"$coef * 1/(1 + ${e.e}*cos(teta))"
          case h:HyperbolaOrbit =>
            val coef = h.a*(h.e-1)*(1+h.e)
            s"$coef * 1/(1 + ${h.e}*cos(teta))"
          case _ => "N/A"
        }
      case None => "N/A"
    }

    strings(2) = s"Уравнение движения: $equation"*/
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "O"
}
