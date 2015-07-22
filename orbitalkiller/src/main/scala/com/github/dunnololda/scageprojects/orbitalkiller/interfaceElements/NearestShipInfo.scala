package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller._
import OrbitalKiller._

class NearestShipInfo extends InterfaceElement {
  private val strings = Array("", "")
  strings(0) = "Расстояние, скорость и фазовый угол относительно ближайшего корабля:"

  override protected def _update(): Unit = {
    val other_ship_near = OrbitalKiller.ship.otherShipsNear.headOption
    other_ship_near match {
      case Some(os) =>
        val (phase_angle_deg, need_orbit_period1, need_orbit_period2) = (for {
          (our_orbit_planet, our_orbit_planet_state) <- insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates)
          (os_orbit_planet, os_orbit_planet_state) <- insideSphereOfInfluenceOfCelestialBody(os.coord, os.mass, currentPlanetStates)
          if our_orbit_planet.index == os_orbit_planet.index
          our_orbit = calculateOrbit(our_orbit_planet_state.mass, our_orbit_planet_state.coord, ship.mass, ship.coord - our_orbit_planet_state.coord, ship.linearVelocity - our_orbit_planet_state.vel, G)
          os_orbit = calculateOrbit(os_orbit_planet_state.mass, os_orbit_planet_state.coord, os.mass, os.coord - os_orbit_planet_state.coord, os.linearVelocity - os_orbit_planet_state.vel, G)
          if our_orbit.isInstanceOf[EllipseOrbit] && os_orbit.isInstanceOf[EllipseOrbit]
          our_orbit_period = our_orbit.asInstanceOf[EllipseOrbit].t
          os_orbit_period = os_orbit.asInstanceOf[EllipseOrbit].t
        } yield {
            val a = (ship.coord - our_orbit_planet_state.coord).deg360(os.coord - our_orbit_planet_state.coord)
            val t1 = a/360.0*os_orbit_period
            val t2 = (a + 360)/360.0*os_orbit_period
            (f"$a%.2f град.", s"${timeStr(t1.toLong*1000)}", s"${timeStr(t2.toLong*1000)}")
        }).getOrElse("N/A", "N/A", "N/A")
        val dist = mOrKm(ship.coord.dist(os.coord))
        val vel = msecOrKmsec((ship.linearVelocity - os.linearVelocity)* (ship.coord - os.coord).n)
        strings(1) = s"$dist, $vel, $phase_angle_deg, $need_orbit_period1, $need_orbit_period2"
      case None =>
        strings(1) = "N/A"
    }
  }
  override def data: Seq[String] = strings

  override val shortDescr: String = "NS"
}
