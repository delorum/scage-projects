package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import scala.collection.{Set, mutable}

object OrbitDataUpdater {
  private val w = earth.radius * scale / 2f

  private def drawStringInOrbitPoint(str:String, deg:Double, o:KeplerOrbit, orbit_color:ScageColor): Unit = {
    openglLocalTransform {
      openglMove(o.orbitalPointByTrueAnomalyDeg(deg) * scale)
      drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
      print(str, Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
    }
  }

  private def printCalculatedData(flight_time_msec:Long, orbital_point:DVec, mouse_teta_rad2Pi:Double, o:KeplerOrbit, planet_radius:Double, orbit_color:ScageColor): Unit = {
    val flight_time_str = s"${timeStr(flight_time_msec)}"
    openglLocalTransform {
      openglMove(orbital_point * scale)
      val vnorm = o.orbitalVelocityValueByTrueAnomalyRad(mouse_teta_rad2Pi)
      print(s"  $flight_time_str : ${mOrKmOrMKm(o.distanceByTrueAnomalyRad(mouse_teta_rad2Pi) - planet_radius)} : ${msecOrKmsec(vnorm)}", Vec.zero, size = (max_font_size / globalScale).toFloat, orbit_color)
    }
  }

  private def drawFuturePositions(maybe_flight_time_msec:Option[Long]): Unit = {
    InterfaceHolder.shipInterfaces.filter(si => {
      !si.isMinimized && !si.monitoring_ship.isCrashed && !player_ship.isDockedToShip(si.monitoring_ship)
    }).flatMap(_.monitoring_ship.thisOrActualProxyShipOrbitData).foreach(x => {
      val ship_orbit = x.orbit.withNewFocusPosition(x.planet_state.coord)
      maybe_flight_time_msec.foreach(flight_time_msec => {
        val position_after_time = ship_orbit.orbitalPointAfterTime(x.body_state.coord, flight_time_msec, x.ccw)
        drawCircle(position_after_time * scale, w/globalScale, YELLOW)
      })
      if (_stop_after_number_of_tacts > 0) {
        val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
        val position_when_stop_moment = ship_orbit.orbitalPointAfterTime(x.body_state.coord, time_to_stop_msec, x.ccw)
        drawCircle(position_when_stop_moment * scale, w/globalScale, GREEN)
      }
    })
    if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == earth.index)) {
      moon.orbitRender.foreach(x => {
        val moon_orbit = x.orbit.withNewFocusPosition(x.planet_state.coord)
        maybe_flight_time_msec.foreach(flight_time_msec => {
          val position_after_time = moon_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, flight_time_msec)
          drawCircle(position_after_time * scale, moon.radius * scale, YELLOW)
          drawCircle(position_after_time * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
        })
        if (_stop_after_number_of_tacts > 0) {
          val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
          val position_when_stop_moment = moon_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, time_to_stop_msec)
          drawCircle(position_when_stop_moment * scale, moon.radius * scale, GREEN)
          drawCircle(position_when_stop_moment * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
        }
      })
    } else if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == sun.index)) {
      earth.orbitRender.foreach(x => {
        val earth_orbit = x.orbit.withNewFocusPosition(x.planet_state.coord)
        maybe_flight_time_msec.foreach(flight_time_msec => {
          val position_after_time = earth_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, flight_time_msec)
          drawCircle(position_after_time * scale, earth.radius * scale, YELLOW)
          drawCircle(position_after_time * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
        })
        if (_stop_after_number_of_tacts > 0) {
          val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
          val position_when_stop_moment = earth_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, time_to_stop_msec)
          drawCircle(position_when_stop_moment * scale, earth.radius * scale, GREEN)
          drawCircle(position_when_stop_moment * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
        }
      })
    }
  }

  private def hyperbolaOrbitDataForNonPlayerShip(update_count:Long, 
                                                 bs: MutableBodyState, 
                                                 body_radius:Double, 
                                                 planet_state:MutableBodyState, 
                                                 planet:CelestialBody, 
                                                 o:HyperbolaOrbit,
                                                 ccw:Boolean, 
                                                 yy:List[DVec], 
                                                 orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, o, ccw, () => {
      openglLocalTransform {
        openglMove(planet_state.coord * scale)
        drawSlidingLines(yy, orbit_color)
      }
      if(InterfaceHolder.namesSwitcher.showNames) {
        val new_o = o.withNewFocusPosition(planet_state.coord)
        drawStringInOrbitPoint("P", 0, new_o, orbit_color)
      }
    })
  }
  
  private def hyperbolaOrbitDataForPlanet(update_count:Long, 
                                      bs: MutableBodyState, 
                                      body_radius:Double, 
                                      planet_state:MutableBodyState, 
                                      planet:CelestialBody, 
                                      o:HyperbolaOrbit,
                                      ccw:Boolean, 
                                      yy:List[DVec], 
                                      orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, o, ccw, () => {
      openglLocalTransform {
        openglMove(planet_state.coord * scale)
        drawSlidingLines(yy, orbit_color)
      }
    })
  }
  
  private def hyperbolaOrbitDataForPlayerShip(update_count:Long, 
                                          bs: MutableBodyState, 
                                          body_radius:Double, 
                                          planet_state:MutableBodyState, 
                                          planet:CelestialBody, 
                                          o:HyperbolaOrbit,
                                          ccw:Boolean, 
                                          yy:List[DVec], 
                                          orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, o, ccw, () => {
      val new_o = o.withNewFocusPosition(planet_state.coord)
      openglLocalTransform {
        openglMove(planet_state.coord * scale)
        drawSlidingLines(yy, orbit_color)
        if(InterfaceHolder.realTrajectorySwitcher.showRealTrajectory && RealTrajectory.realTrajectory.nonEmpty) {
          drawSlidingLines(RealTrajectory.realTrajectory, orbit_color)
          /*RealTrajectory.realTrajectory.foreach(p => {
            drawFilledCircle(p, 3 / globalScale, orbit_color)
          })*/
          drawSlidingLines(RealTrajectory2.realTrajectory, ORANGE)
          drawSlidingLines(RealTrajectory3.realTrajectory, ORANGE_RED)
        }
      }
      /*drawLine(new_o.f*scale, new_o.center*scale, GRAY)
      if(_stop_after_number_of_tacts > 0) {
        val p = new_o.orbitalPointAfterTime(bs.coord, (_stop_after_number_of_tacts * base_dt * 1000).toLong, ccw)
        drawLine(p*scale, (p + (new_o.center - new_o.f).perpendicular*math.signum(p*(new_o.center - new_o.f)))*scale, GRAY)
        val p2 = new_o.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly)
        drawLine(p2*scale, (p2 + (new_o.center - new_o.f).perpendicular*math.signum(p*(new_o.center - new_o.f)))*scale, GRAY)
      }*/


      if(InterfaceHolder.namesSwitcher.showNames) {
        drawStringInOrbitPoint("P", 0, new_o, orbit_color)
      }
      val mouse_point = absCoord(mouseCoord) / scale
      drawLine(new_o.f * scale, mouse_point * scale, DARK_GRAY)

      if (_stop_after_number_of_tacts > 0) {
        drawFilledCircle(new_o.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly) * scale, 3 / globalScale, RED)
        drawFilledCircle(new_o.orbitalPointAfterTime(bs.coord, (_stop_after_number_of_tacts * base_dt * 1000).toLong, ccw) * scale, 3 / globalScale, GREEN)
      }

      val mouse_teta_rad2Pi = new_o.tetaRad2PiInPoint(mouse_point)
      val ship_teta_rad2Pi = new_o.tetaRad2PiInPoint(bs.coord)
      if (new_o.tetaRad2PiValid(mouse_teta_rad2Pi)) {
        val away_from_rp = (bs.coord - new_o.f) * (bs.vel - planet_state.vel) >= 0 // приближаемся к перигею или удаляемся от него?

        val mouse_point_further_on_the_way = {
          if (ccw) {
            (away_from_rp && ship_teta_rad2Pi <= mouse_teta_rad2Pi && mouse_teta_rad2Pi <= new_o.teta_rad_min) ||
              (!away_from_rp && ((ship_teta_rad2Pi <= mouse_teta_rad2Pi && mouse_teta_rad2Pi <= 360) || (0 <= mouse_teta_rad2Pi && mouse_teta_rad2Pi <= new_o.teta_rad_min)))
          } else {
            (away_from_rp && ship_teta_rad2Pi >= mouse_teta_rad2Pi && mouse_teta_rad2Pi >= new_o.teta_rad_max) ||
              (!away_from_rp && ((ship_teta_rad2Pi >= mouse_teta_rad2Pi && mouse_teta_rad2Pi >= 0) || (360 >= mouse_teta_rad2Pi && mouse_teta_rad2Pi >= new_o.teta_rad_max)))
          }
        }

        if (mouse_point_further_on_the_way) {
          val orbital_point = new_o.orbitalPointInPoint(mouse_point)
          drawFilledCircle(orbital_point * scale, 3 / globalScale, orbit_color)
          lazy val flight_time_msec = new_o.travelTimeOnOrbitMsec(bs.coord, orbital_point, ccw)

          if (set_stop_moment) {
            _stop_after_number_of_tacts = (flight_time_msec / 1000 / base_dt).toLong
            _stop_in_orbit_true_anomaly = mouse_teta_rad2Pi
            set_stop_moment = false
          }

          if (InterfaceHolder.orbParams.calculationOn) {
            printCalculatedData(flight_time_msec, orbital_point, mouse_teta_rad2Pi, o, planet.radius, orbit_color:ScageColor)
            drawFuturePositions(Some(flight_time_msec))
          }
        } else if (InterfaceHolder.orbParams.calculationOn && _stop_after_number_of_tacts > 0) {
          drawFuturePositions(None)
        }
      }
    })
  }
  
  private def ellipseOrbitDataForNonPlayerShip(update_count:Long, 
                                           bs: MutableBodyState, 
                                           body_radius:Double, 
                                           planet_state:MutableBodyState, 
                                           planet:CelestialBody, 
                                           o:EllipseOrbit,
                                           ccw:Boolean,
                                           orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, o, ccw, () => {
      val new_center = o.centerIfFocusPosition(planet_state.coord)
      openglLocalTransform {
        openglMove(new_center * scale)
        if(o.f2 != o.f) openglRotateDeg(Vec(1, 0).signedDeg(o.f_minus_f2_n))
        drawEllipse(DVec.zero, o.a * scale, o.b * scale, orbit_color)
      }
      if(InterfaceHolder.namesSwitcher.showNames) {
        val new_o = o.withNewFocusPosition(planet_state.coord)
        drawStringInOrbitPoint("P", 0, new_o, orbit_color)
        drawStringInOrbitPoint("A", 180, new_o, orbit_color)
      }
    })
  }
  
  private def ellipseOrbitDataForPlanet(update_count:Long, 
                                        bs: MutableBodyState, 
                                        body_radius:Double, 
                                        planet_state:MutableBodyState, 
                                        planet:CelestialBody, 
                                        o:EllipseOrbit,
                                        ccw:Boolean,
                                        orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, o, ccw, () => {
      val new_center = o.centerIfFocusPosition(planet_state.coord)
      openglLocalTransform {
        openglMove(new_center * scale)
        if(o.f2 != o.f) openglRotateDeg(Vec(1, 0).signedDeg(o.f_minus_f2_n))
        drawEllipse(DVec.zero, o.a * scale, o.b * scale, orbit_color)
      }
    })
  }
  
  private def ellipseOrbitDataForPlayerShip(update_count:Long, 
                                            bs: MutableBodyState, 
                                            body_radius:Double, 
                                            planet_state:MutableBodyState, 
                                            planet:CelestialBody, 
                                            o:EllipseOrbit,
                                            ccw:Boolean,
                                            orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, o, ccw, () => {
      val new_o = o.withNewFocusPosition(planet_state.coord)
      openglLocalTransform {
        openglMove(new_o.center * scale)
        if(new_o.f2 != new_o.f) openglRotateDeg(Vec(-1, 0).signedDeg(new_o.f2 - new_o.f))
        drawEllipse(DVec.zero, new_o.a * scale, new_o.b * scale, orbit_color)
      }
      if(InterfaceHolder.realTrajectorySwitcher.showRealTrajectory && RealTrajectory.realTrajectory.nonEmpty) {
        openglLocalTransform {
          openglMove(planet_state.coord * scale)
          drawSlidingLines(RealTrajectory.realTrajectory, orbit_color)
          /*RealTrajectory.realTrajectory.foreach(p => {
            drawFilledCircle(p, 3 / globalScale, orbit_color)
          })*/
          drawSlidingLines(RealTrajectory2.realTrajectory, ORANGE)
          drawSlidingLines(RealTrajectory3.realTrajectory, ORANGE_RED)
        }
      }
      if(InterfaceHolder.namesSwitcher.showNames) {
        drawStringInOrbitPoint("P", 0, new_o, orbit_color)
        drawStringInOrbitPoint("A", 180, new_o, orbit_color)
      }
      val mouse_point = absCoord(mouseCoord) / scale
      drawLine(new_o.f * scale, mouse_point * scale, DARK_GRAY)

      val orbital_point = new_o.orbitalPointInPoint(mouse_point)
      drawFilledCircle(orbital_point * scale, 3 / globalScale, orbit_color)

      if (_stop_after_number_of_tacts > 0) {
        drawFilledCircle(new_o.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly) * scale, 3 / globalScale, RED)
        drawFilledCircle(new_o.orbitalPointAfterTime(bs.coord, (_stop_after_number_of_tacts * base_dt * 1000).toLong, ccw) * scale, 3 / globalScale, GREEN)
      }
      val mouse_teta_rad2Pi = new_o.tetaRad2PiInPoint(mouse_point)

      lazy val flight_time_msec = new_o.travelTimeOnOrbitMsec(bs.coord, orbital_point, ccw)

      if (set_stop_moment) {
        _stop_after_number_of_tacts = (flight_time_msec / 1000 / base_dt).toLong
        _stop_in_orbit_true_anomaly = mouse_teta_rad2Pi
        set_stop_moment = false
      }
      if (InterfaceHolder.orbParams.calculationOn) {
        printCalculatedData(flight_time_msec, orbital_point, mouse_teta_rad2Pi, o, planet.radius, orbit_color:ScageColor)
        drawFuturePositions(Some(flight_time_msec))
      }
    })
  }

  def updateOrbitData(update_count:Long,
                      body_index: Int,
                      body_radius:Double,
                      orbit_color: ScageColor,
                      some_system_state: mutable.Map[Int, MutableBodyState],
                      need_planets: Set[Int],
                      calculate_orbit_around:Option[Int]): Option[OrbitData] = {
    some_system_state.get(body_index).flatMap(bs => {
      updateOrbitData(update_count, bs, body_radius, orbit_color, some_system_state, need_planets, calculate_orbit_around)
    })
  }

  def updateOrbitData(update_count:Long,
                      bs: MutableBodyState,
                      body_radius:Double,
                      orbit_color: ScageColor,
                      some_system_state: mutable.Map[Int, MutableBodyState],
                      need_planets: Set[Int],
                      calculate_orbit_around:Option[Int]): Option[OrbitData] = {
    val celestials = some_system_state.filter(kv => need_planets.contains(kv._1)).flatMap(kv => {
      planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
    }).values.toSeq.sortBy(_._2.mass)
    // смотрим, где он находится
    val bs_coord = bs.coord
    val bs_vel = bs.vel
    val bs_mass = bs.mass
    val bs_ang = bs.ang
    calculate_orbit_around.map(idx => celestials.find(_._1.index == idx)).getOrElse(insideSphereOfInfluenceOfCelestialBody(bs_coord, bs_mass, celestials)) match {
      case Some((planet, planet_state)) =>
        val planet_state_coord = planet_state.coord
        val planet_state_vel = planet_state.vel
        val planet_state_mass = planet_state.mass
        // корабль находится внутри гравитационного радиуса какой-то планеты (Земли или Луны)
        val orbit = calculateOrbit(
          planet_state_mass,
          planet_state_coord,
          bs_mass,
          bs_coord - planet_state_coord,
          bs_vel - planet_state_vel, G)
        val ccw = (bs_coord - orbit.f).perpendicular * (bs_vel - planet_state_vel) >= 0 // летим против часовой?
        orbit match {
          case h: HyperbolaOrbit =>
            val yy = (-(-1.0 / h.e).myacos + 0.1 to (-1.0 / h.e).myacos - 0.1 by 0.1).map(true_anomaly => {
              val r = h.a * (h.e * h.e - 1) / (1 + h.e * math.cos(true_anomaly))
              (h.f_minus_center_n * r).rotateRad(true_anomaly) * scale
            }).toList
            if (bs.index != player_ship.thisOrActualProxyShipIndex) {
              if(ShipsHolder.shipIndicies.contains(bs.index)) {
                Some(hyperbolaOrbitDataForNonPlayerShip(update_count, bs, body_radius, planet_state, planet, h, ccw, yy, orbit_color))
              } else {
                Some(hyperbolaOrbitDataForPlanet(update_count, bs, body_radius, planet_state, planet, h, ccw, yy, orbit_color))
              }
            } else {
              Some(hyperbolaOrbitDataForPlayerShip(update_count, bs, body_radius, planet_state, planet, h, ccw, yy, orbit_color))
            }
          case e: EllipseOrbit =>
            if (bs.index != player_ship.thisOrActualProxyShipIndex) {
              if(ShipsHolder.shipIndicies.contains(bs.index)) {
                Some(ellipseOrbitDataForNonPlayerShip(update_count, bs, body_radius, planet_state, planet, e, ccw, orbit_color))
              } else {
                Some(ellipseOrbitDataForPlanet(update_count, bs, body_radius, planet_state, planet, e, ccw, orbit_color))
              }
            } else {
              Some(ellipseOrbitDataForPlayerShip(update_count, bs, body_radius, planet_state, planet, e, ccw, orbit_color))
            }
        }
      case None => None
    }
  }
}
