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
        val (_, need_orbit_period_str) = (for {
          (our_orbit_planet, our_orbit_planet_state) <- insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates)
          (os_orbit_planet, os_orbit_planet_state) <- insideSphereOfInfluenceOfCelestialBody(os.coord, os.mass, currentPlanetStates)
          if our_orbit_planet.index == os_orbit_planet.index
          our_orbit_kepler = calculateOrbit(our_orbit_planet_state.mass, our_orbit_planet_state.coord, ship.mass, ship.coord - our_orbit_planet_state.coord, ship.linearVelocity - our_orbit_planet_state.vel, G)
          os_orbit_kepler = calculateOrbit(os_orbit_planet_state.mass, os_orbit_planet_state.coord, os.mass, os.coord - os_orbit_planet_state.coord, os.linearVelocity - os_orbit_planet_state.vel, G)
          if our_orbit_kepler.isInstanceOf[EllipseOrbit] && os_orbit_kepler.isInstanceOf[EllipseOrbit]
          our_orbit_ellipse = our_orbit_kepler.asInstanceOf[EllipseOrbit]
          os_orbit_ellipse = os_orbit_kepler.asInstanceOf[EllipseOrbit]
          our_orbit_period = our_orbit_ellipse.t
          os_orbit_period = os_orbit_ellipse.t
        } yield {
            val deg_diff = (os.coord - our_orbit_planet_state.coord).deg360(ship.coord - our_orbit_planet_state.coord)
            val our_r_p = our_orbit_ellipse.orbitalPointByTrueAnomalyDeg(0)
            val our_r_a = our_orbit_ellipse.orbitalPointByTrueAnomalyDeg(180)
            val t1_sec = deg_diff/360.0*os_orbit_period
            val t2_sec = (deg_diff + 360)/360.0*os_orbit_period
            // здесь два варианта орбитальных периодов, один больше другой меньше. Оба могут соответствовать орбитам с нормальной высотой,
            // но также может быть, что меньший период t1 соответствует суборбитальной траектории. Это надо все просчитать.
            val mu = G*our_orbit_planet_state.mass
            def _calculateRp(t_sec:Double):Double = {
              val a = math.pow(math.pow(t_sec/(2*math.Pi), 2)*mu, 1.0/3)  // большая полуось для данного периода
              val x = ship.coord.dist(our_orbit_planet_state.coord)
              math.min(2*a-x, x)
            }
            val r_p1 = _calculateRp(t1_sec) - our_orbit_planet.radius
            val r_p2 = _calculateRp(t2_sec) - our_orbit_planet.radius
            val sep_in_r_p = os_orbit_ellipse.orbitalPointInPoint(our_r_p).dist(our_r_p)
            val sep_in_r_a = os_orbit_ellipse.orbitalPointInPoint(our_r_a).dist(our_r_a)
            val sep_str = if(sep_in_r_p <= sep_in_r_a) {
              s"min sep = in r_p, ${mOrKmOrMKm(sep_in_r_p)}"
            } else {
              s"min sep = in r_a, ${mOrKmOrMKm(sep_in_r_a)}"
            }
            val cur_sep = os_orbit_ellipse.orbitalPointInPoint(ship.coord).dist(ship.coord)
            val need_orbit_period_str = {
              if(r_p1 >= our_orbit_planet.air_free_altitude) {
                if(r_p2 >= our_orbit_planet.air_free_altitude) {
                  if(math.abs(our_orbit_ellipse.t - t1_sec) <= math.abs(our_orbit_ellipse.t - t2_sec)) {
                    s"${timeStr(t1_sec.toLong*1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                  } else {
                    s"${timeStr(t2_sec.toLong*1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                  }
                } else {
                  s"${timeStr(t1_sec.toLong*1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                }
              } else {
                if(r_p2 >= our_orbit_planet.air_free_altitude) {
                  s"${timeStr(t2_sec.toLong*1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                } else {
                  s"N/A ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                }
              }
            }
            (f"$deg_diff%.2f град.", need_orbit_period_str)
        }).getOrElse("N/A", "N/A")
        val dist = mOrKmOrMKm(ship.coord.dist(os.coord))
        val vel = msecOrKmsec((ship.linearVelocity - os.linearVelocity)* (ship.coord - os.coord).n)
        strings(1) = s"$dist, $vel, $need_orbit_period_str"
      case None =>
        strings(1) = "N/A"
    }
  }
  override def data: Seq[String] = strings
  override val color = ScageColor.MAGENTA

  override val shortDescr: String = "NS"
}
