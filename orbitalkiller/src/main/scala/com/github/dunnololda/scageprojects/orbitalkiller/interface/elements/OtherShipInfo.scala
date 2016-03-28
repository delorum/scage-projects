package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import OrbitalKiller._

class OtherShipInfo(val monitoring_ship:PolygonShip) extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    val (_, need_orbit_period_str) = (for {
      (our_orbit_planet, our_orbit_planet_state) <- insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates)
      (os_orbit_planet, os_orbit_planet_state) <- insideSphereOfInfluenceOfCelestialBody(monitoring_ship.coord, monitoring_ship.mass, currentPlanetStates)
      if our_orbit_planet.index == os_orbit_planet.index
      our_orbit_kepler = calculateOrbit(our_orbit_planet_state.mass, our_orbit_planet_state.coord, ship.mass, ship.coord - our_orbit_planet_state.coord, ship.linearVelocity - our_orbit_planet_state.vel, G)
      os_orbit_kepler = calculateOrbit(os_orbit_planet_state.mass, os_orbit_planet_state.coord, monitoring_ship.mass, monitoring_ship.coord - os_orbit_planet_state.coord, monitoring_ship.linearVelocity - os_orbit_planet_state.vel, G)
      if our_orbit_kepler.isInstanceOf[EllipseOrbit] && os_orbit_kepler.isInstanceOf[EllipseOrbit]
      our_orbit_ellipse = our_orbit_kepler.asInstanceOf[EllipseOrbit]
      os_orbit_ellipse = os_orbit_kepler.asInstanceOf[EllipseOrbit]
      //our_orbit_period = our_orbit_ellipse.t
      os_orbit_period = os_orbit_ellipse.t
    } yield {
        if(InterfaceHolder.dockingSwitcher.dockingEnabled && ship.coord.dist(monitoring_ship.coord) <= 2000) {
          val ship_docking_point = ship.docking_points.head.curP1 + 0.5*(ship.docking_points.head.curP2 - ship.docking_points.head.curP1)
          monitoring_ship.docking_points.sortBy(osdp => osdp.curP1.dist(ship_docking_point)).headOption match {
            case Some(osdp) =>
              val vv1 = (osdp.curP1-osdp.curP2).n
              val docking_point = osdp.curP1 + 0.5*(osdp.curP2 - osdp.curP1)
              val docking_dir = if(osdp.p1*vv1.perpendicular < 0) vv1.perpendicular else -vv1.perpendicular
              val A = ship_docking_point.x
              val B = ship_docking_point.y
              val C = docking_point.x
              val D = docking_point.y
              val a1 = vv1.x
              val a2 = vv1.y
              val b1 = docking_dir.x
              val b2 = docking_dir.y
              // координаты точки стыковки корабля в системе координат с началом в docking_point и базисными векторами (vv1, docking_dir)
              val x = (b2*(A-C) - b1*(B-D))/(a1*b2 - a2*b1)
              val y = (a2*(A-C) - a1*(B-D))/(a2*b1 - a1*b2)
              ("N/A", f"docking data: x=$x%.2f y=$y%.2f м")
            case None =>
              ("N/A", "N/A")
          }
        } else {
          val deg_diff = (monitoring_ship.coord - our_orbit_planet_state.coord).deg360(ship.coord - our_orbit_planet_state.coord)
          val our_r_p = our_orbit_ellipse.orbitalPointByTrueAnomalyDeg(0)
          val our_r_a = our_orbit_ellipse.orbitalPointByTrueAnomalyDeg(180)
          val t1_sec = deg_diff / 360.0 * os_orbit_period
          val t2_sec = (deg_diff + 360) / 360.0 * os_orbit_period
          // здесь два варианта орбитальных периодов, один больше другой меньше. Оба могут соответствовать орбитам с нормальной высотой,
          // но также может быть, что меньший период t1 соответствует суборбитальной траектории. Это надо все просчитать.
          val mu = G * our_orbit_planet_state.mass
          def _calculateRp(t_sec: Double): Double = {
            val a = math.pow(math.pow(t_sec / (2 * math.Pi), 2) * mu, 1.0 / 3) // большая полуось для данного периода
            val x = ship.coord.dist(our_orbit_planet_state.coord)
            math.min(2 * a - x, x)
          }
          val r_p1 = _calculateRp(t1_sec) - our_orbit_planet.radius
          val r_p2 = _calculateRp(t2_sec) - our_orbit_planet.radius
          val sep_in_r_p = os_orbit_ellipse.orbitalPointInPoint(our_r_p).dist(our_r_p)
          val sep_in_r_a = os_orbit_ellipse.orbitalPointInPoint(our_r_a).dist(our_r_a)
          val sep_str = if (sep_in_r_p <= sep_in_r_a) {
            s"min sep = in r_p, ${mOrKmOrMKm(sep_in_r_p)}"
          } else {
            s"min sep = in r_a, ${mOrKmOrMKm(sep_in_r_a)}"
          }
          val cur_sep = os_orbit_ellipse.orbitalPointInPoint(ship.coord).dist(ship.coord)
          val need_orbit_period_str = {
            if (r_p1 >= our_orbit_planet.air_free_altitude) {
              if (r_p2 >= our_orbit_planet.air_free_altitude) {
                if (math.abs(our_orbit_ellipse.t - t1_sec) <= math.abs(our_orbit_ellipse.t - t2_sec)) {
                  s"${timeStr(t1_sec.toLong * 1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                } else {
                  s"${timeStr(t2_sec.toLong * 1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
                }
              } else {
                s"${timeStr(t1_sec.toLong * 1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
              }
            } else {
              if (r_p2 >= our_orbit_planet.air_free_altitude) {
                s"${timeStr(t2_sec.toLong * 1000)} ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
              } else {
                s"N/A ($sep_str, cur sep = ${mOrKmOrMKm(cur_sep)})"
              }
            }
          }
          (f"$deg_diff%.2f град.", s"rendezvous data: $need_orbit_period_str")
        }
      }).getOrElse("N/A", "N/A")
    val dist = mOrKmOrMKm(ship.coord.dist(monitoring_ship.coord))
    val vel = msecOrKmsec((ship.linearVelocity - monitoring_ship.linearVelocity)* (ship.coord - monitoring_ship.coord).n)
    strings(0) = s"${monitoring_ship.name}: dist=$dist, vel=$vel, $need_orbit_period_str"
  }
  override def data: Seq[String] = strings
  override val color = ScageColor.MAGENTA

  override val shortDescr: String = "OS"
}
