package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scage.support.ScageColor
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
            val deg_diff = (os.coord - our_orbit_planet_state.coord).deg360(ship.coord - our_orbit_planet_state.coord)
            val t1_sec = deg_diff/360.0*os_orbit_period
            val t2_sec = (deg_diff + 360)/360.0*os_orbit_period
            // здесь два варианта орбитальных периодов, один больше другой меньше. Оба могут соответствовать орбитам с нормальной высотой,
            // но также может быть, что меньший период t1 соответствует суборбитальной траектории. Это надо все просчитать.
            val mu = G*our_orbit_planet_state.mass
            def _calculateRp(t_sec:Double):Double = {
              val a = math.pow(math.pow(t_sec/(2*math.Pi), 2)*mu, 1.0/3)  // большая полуось для данного периода
              val x = ship.coord.dist(our_orbit_planet_state.coord)
              2*a-x-our_orbit_planet.radius
            }
            val r_p1 = math.min(_calculateRp(t1_sec), ship.coord.dist(our_orbit_planet_state.coord) - our_orbit_planet.radius)
            val r_p2 = math.min(_calculateRp(t2_sec), ship.coord.dist(our_orbit_planet_state.coord) - our_orbit_planet.radius)
            (f"$deg_diff%.2f град.", s"${timeStr(t1_sec.toLong*1000)} (r_p=${mOrKm(r_p1)})", s"${timeStr(t2_sec.toLong*1000)} (r_p=${mOrKm(r_p2)})")
        }).getOrElse("N/A", "N/A", "N/A")
        val dist = mOrKm(ship.coord.dist(os.coord))
        val vel = msecOrKmsec((ship.linearVelocity - os.linearVelocity)* (ship.coord - os.coord).n)
        strings(1) = s"$dist, $vel, $phase_angle_deg, $need_orbit_period1, $need_orbit_period2"
      case None =>
        strings(1) = "N/A"
    }
  }
  override def data: Seq[String] = strings
  override val color = ScageColor.MAGENTA

  override val shortDescr: String = "NS"
}
