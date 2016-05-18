package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.{DVec, ScageColor}
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class OtherShipInfo(val monitoring_ship: PolygonShip) extends InterfaceElement {
  private val strings = Array("")

  def forceUpdate(): Unit = {
    _update_needed = true
    _update()
  }

  override protected def _update(): Unit = {
    if(_update_needed) {
      if (monitoring_ship.isDead) {
        strings(0) = s"${monitoring_ship.name}: ${monitoring_ship.deathReason}"
      } else {
        if (player_ship.isDockedToShip(monitoring_ship)) {
          strings(0) = s"${monitoring_ship.name}: docked"
        } else {
          val /*(_, */ need_orbit_period_str /*)*/ = (for {
            OrbitData(_, _, _, _, _, _, _, our_orbit_planet, our_orbit_kepler, our_ccw, _) <- player_ship.orbitData
            OrbitData(_, _, _, _, _, _, _, os_orbit_planet, os_orbit_kepler, os_ccw, _) <- monitoring_ship.orbitData
            if our_orbit_planet.index == os_orbit_planet.index
            if our_ccw == os_ccw
            our_orbit_ellipse <- player_ship.orbitData.flatMap(_.ellipseOrbit)
            os_orbit_ellipse <- monitoring_ship.orbitData.flatMap(_.ellipseOrbit)
            our_orbit_period_sec = our_orbit_ellipse.t
            os_orbit_period_sec = os_orbit_ellipse.t
          } yield {
              if (InterfaceHolder.dockingSwitcher.dockingEnabled && player_ship.coord.dist(monitoring_ship.coord) <= 2000) {
                val ship_docking_point = player_ship.docking_points.head.curP1 + 0.5 * (player_ship.docking_points.head.curP2 - player_ship.docking_points.head.curP1)
                monitoring_ship.docking_points.sortBy(osdp => osdp.curP1.dist(ship_docking_point)).headOption match {
                  case Some(osdp) =>
                    val vv1 = (osdp.curP1 - osdp.curP2).n
                    val docking_point = osdp.curP1 + 0.5 * (osdp.curP2 - osdp.curP1)
                    val docking_dir = -vv1.perpendicular
                    val A = ship_docking_point.x
                    val B = ship_docking_point.y
                    val C = docking_point.x
                    val D = docking_point.y
                    val a1 = vv1.x
                    val a2 = vv1.y
                    val b1 = docking_dir.x
                    val b2 = docking_dir.y
                    // координаты точки стыковки корабля в системе координат с началом в docking_point и базисными векторами (vv1, docking_dir)
                    val angle = DVec(0, 1).deg360(docking_dir) - player_ship.rotation
                    val x = -(b2 * (A - C) - b1 * (B - D)) / (a1 * b2 - a2 * b1)
                    val y = (a2 * (A - C) - a1 * (B - D)) / (a2 * b1 - a1 * b2)
                    /*("N/A", */ f"docking data: a=$angle%.2f x=$x%.2f y=$y%.2f" /*)*/
                  case None =>
                    /*("N/A", */ "N/A" /*)*/
                }
              } else {
                val os_travel_time_to_our_point1_msec = os_orbit_ellipse.travelTimeOnOrbitMsec(monitoring_ship.coord, os_orbit_ellipse.orbitalPointInPoint(player_ship.coord), os_ccw)
                val os_travel_time_to_our_point2_msec = (os_orbit_period_sec * 1000 - os_travel_time_to_our_point1_msec).toLong
                val need_orbit_period_msec = {
                  if (os_travel_time_to_our_point1_msec > os_travel_time_to_our_point2_msec) {
                    val mu = G * our_orbit_planet.mass
                    def _calculateRp(t_sec: Double): Double = {
                      val a = math.pow(math.pow(t_sec / (2 * math.Pi), 2) * mu, 1.0 / 3) // большая полуось для данного периода
                      val x = player_ship.coord.dist(our_orbit_planet.coord)
                      math.min(2 * a - x, x)
                    }
                    val r_p1 = _calculateRp(os_travel_time_to_our_point1_msec / 1000) - our_orbit_planet.radius
                    if (r_p1 > our_orbit_planet.air_free_altitude) os_travel_time_to_our_point1_msec
                    else os_travel_time_to_our_point1_msec * 2 + os_travel_time_to_our_point2_msec
                  } else os_travel_time_to_our_point1_msec * 2 + os_travel_time_to_our_point2_msec
                }
                /*val deg_diff = (monitoring_ship.coord - our_orbit_planet_state.coord).deg360(player_ship.coord - our_orbit_planet_state.coord)
              val t1_sec = deg_diff / 360.0 * os_orbit_period
              val t2_sec = t1_sec + os_orbit_period*/
                // здесь два варианта орбитальных периодов, один больше другой меньше. Оба могут соответствовать орбитам с нормальной высотой,
                // но также может быть, что меньший период t1 соответствует суборбитальной траектории. Это надо все просчитать.
                /*val mu = G * our_orbit_planet_state.mass
              def _calculateRp(t_sec: Double): Double = {
                val a = math.pow(math.pow(t_sec / (2 * math.Pi), 2) * mu, 1.0 / 3) // большая полуось для данного периода
                val x = player_ship.coord.dist(our_orbit_planet_state.coord)
                math.min(2 * a - x, x)
              }
              val r_p1 = _calculateRp(t1_sec) - our_orbit_planet.radius
              val r_p2 = _calculateRp(t2_sec) - our_orbit_planet.radius*/
                /*val our_r_p = our_orbit_ellipse.orbitalPointByTrueAnomalyDeg(0) // координаты апогея нашей орбиты
              val our_r_a = our_orbit_ellipse.orbitalPointByTrueAnomalyDeg(180) // координаты перигея нашей орбиты
              val sep_in_r_p = os_orbit_ellipse.orbitalPointInPoint(our_r_p).dist(our_r_p)
              val sep_in_r_a = os_orbit_ellipse.orbitalPointInPoint(our_r_a).dist(our_r_a)*/
                val cur_sep = os_orbit_ellipse.orbitalPointInPoint(player_ship.coord).dist(player_ship.coord)
                /*val sep_str = if (sep_in_r_p <= sep_in_r_a) {
                s"min sep = in r_p, ${mOrKmOrMKm(sep_in_r_p)}, cur sep = ${mOrKmOrMKm(cur_sep)}"
              } else {
                s"min sep = in r_a, ${mOrKmOrMKm(sep_in_r_a)}, cur sep = ${mOrKmOrMKm(cur_sep)}"
              }*/
                val sep_str = s"cur sep = ${mOrKmOrMKm(cur_sep)}"
                val need_orbit_period_str = {
                  /*if (r_p1 >= our_orbit_planet.air_free_altitude) {
                  if (r_p2 >= our_orbit_planet.air_free_altitude) {
                    if (math.abs(our_orbit_period - t1_sec) <= math.abs(our_orbit_period - t2_sec)) {
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
                }*/
                  s"${timeStr(need_orbit_period_msec)} ($sep_str)"
                }
                /*(f"$deg_diff%.2f град.", */ s"rendezvous data: $need_orbit_period_str" /*)*/
              }
            }).getOrElse(/*"N/A", */ "N/A")
          val dist = mOrKmOrMKm(player_ship.coord.dist(monitoring_ship.coord))
          val vel = msecOrKmsec((player_ship.linearVelocity - monitoring_ship.linearVelocity) * (player_ship.coord - monitoring_ship.coord).n)
          strings(0) = s"${monitoring_ship.name}: dist=$dist, vel=$vel, $need_orbit_period_str"
        }
      }
      _update_needed = false
    }
  }

  override def data: Seq[String] = strings

  override val color = ScageColor.MAGENTA

  override val shortDescr: String = "OS"
}
