package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import scala.collection.{Set, mutable}

object OrbitDataUpdater {
  private val w = earth.radius * scale / 2f

  private def drawStringInOrbitPoint(str:String, deg:Double, o:KeplerOrbit, orbit_color:ScageColor): Unit = {
    openglLocalTransform {
      openglMove(o.orbitalPointByTrueAnomalyDeg(0) * scale)
      drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
      print(str, Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
    }
  }

  private def hyperbolaOrbitDataForNonPlayerShip(update_count:Long, 
                                                 bs: MutableBodyState, 
                                                 body_radius:Double, 
                                                 planet_state:MutableBodyState, 
                                                 planet:CelestialBody, 
                                                 h:HyperbolaOrbit, 
                                                 ccw:Boolean, 
                                                 yy:List[DVec], 
                                                 orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, h, ccw, () => {
      val new_h = h.withNewFocusPosition(planet_state.coord)
      drawSlidingLines(yy.map(_ + planet_state.coord * scale), orbit_color)
      if(InterfaceHolder.namesSwitcher.showNames) {
        openglLocalTransform {
          openglMove(new_h.orbitalPointByTrueAnomalyDeg(0) * scale)
          drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
          print("P", Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
        }
      }
    })
  }
  
  private def hyperbolaOrbitDataForPlanet(update_count:Long, 
                                      bs: MutableBodyState, 
                                      body_radius:Double, 
                                      planet_state:MutableBodyState, 
                                      planet:CelestialBody, 
                                      h:HyperbolaOrbit, 
                                      ccw:Boolean, 
                                      yy:List[DVec], 
                                      orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, h, ccw, () => {
      drawSlidingLines(yy.map(_ + planet_state.coord * scale), orbit_color)
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
      drawSlidingLines(yy.map(_ + planet_state.coord * scale), orbit_color)
      if(InterfaceHolder.namesSwitcher.showNames) {
        openglLocalTransform {
          openglMove(new_o.orbitalPointByTrueAnomalyDeg(0) * scale)
          drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
          print("P", Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
        }
      }
      val mouse_point = absCoord(mouseCoord) / scale
      drawLine(new_o.f * scale, mouse_point * scale, DARK_GRAY)

      if (_stop_after_number_of_tacts > 0) {
        //drawFilledCircle(h.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly) * scale, 3 / globalScale, RED)
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
            set_stop_moment = false

            /*val p1 = h.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly)
            println((50 to 300 by 50).map(num_iterations => {
              val px = h.orbitalPointAfterTime(bs.coord, (_stop_after_number_of_tacts*base_dt).toLong, ccw, num_iterations)
              mOrKmOrMKm(p1.dist(px))
            }).mkString(" : "))*/
          }

          if (InterfaceHolder.orbParams.calculationOn) {
            val flight_time = s"${timeStr(flight_time_msec)}"
            val vnorm = new_o.orbitalVelocityByTrueAnomalyRad(mouse_teta_rad2Pi)
            openglLocalTransform {
              openglMove(orbital_point * scale)
              print(s"  $flight_time : ${mOrKmOrMKm(new_o.distanceByTrueAnomalyRad(mouse_teta_rad2Pi) - planet.radius)} : ${msecOrKmsec(vnorm)}", Vec.zero, size = (max_font_size / globalScale).toFloat, orbit_color)
            }
            InterfaceHolder.shipInterfaces.filter(si => {
              !si.isMinimized && !si.monitoring_ship.isCrashed && !player_ship.isDockedToShip(si.monitoring_ship)
            }).flatMap(_.monitoring_ship.thisOrActualProxyShipOrbitData).foreach(x => {
              x.ellipseOrbit.foreach(ship_orbit => {
                val position_after_time = ship_orbit.orbitalPointAfterTime(x.body_state.coord, flight_time_msec, x.ccw)
                drawCircle(position_after_time * scale, w/globalScale, YELLOW)
              })
            })
            if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == earth.index)) {
              moon.orbitRender.foreach(x => {
                x.ellipseOrbit.foreach(moon_orbit => {
                  val position_after_time = moon_orbit.orbitalPointAfterTime(x.body_state.coord, flight_time_msec, x.ccw)
                  drawCircle(position_after_time * scale, moon.radius * scale, YELLOW)
                  drawCircle(position_after_time * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
                })
              })
            } else if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == sun.index)) {
              earth.orbitRender.foreach(x => {
                x.ellipseOrbit.foreach(earth_orbit => {
                  val position_after_time = earth_orbit.orbitalPointAfterTime(x.body_state.coord, flight_time_msec, x.ccw)
                  drawCircle(position_after_time * scale, earth.radius * scale, YELLOW)
                  drawCircle(position_after_time * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
                })
              })
            }
          }
        }
        if (InterfaceHolder.orbParams.calculationOn && _stop_after_number_of_tacts > 0) {
          InterfaceHolder.shipInterfaces.filter(si => {
            !si.isMinimized && !si.monitoring_ship.isCrashed && !player_ship.isDockedToShip(si.monitoring_ship)
          }).flatMap(_.monitoring_ship.thisOrActualProxyShipOrbitData).foreach(x => {
            x.ellipseOrbit.foreach(ship_orbit => {
              val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
              val position_when_stop_moment = ship_orbit.orbitalPointAfterTime(x.body_state.coord, time_to_stop_msec, x.ccw)
              drawCircle(position_when_stop_moment * scale, w/globalScale, GREEN)
            })
          })
          if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == earth.index)) {
            moon.orbitRender.foreach(x => {
              x.ellipseOrbit.foreach(moon_orbit => {
                val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
                val position_when_stop_moment = moon_orbit.orbitalPointAfterTime(x.body_state.coord, time_to_stop_msec, x.ccw)
                drawCircle(position_when_stop_moment * scale, moon.radius * scale, GREEN)
                drawCircle(position_when_stop_moment * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
              })
            })
          } else if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == sun.index)) {
            earth.orbitRender.foreach(x => {
              x.ellipseOrbit.foreach(earth_orbit => {
                val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
                val position_when_stop_moment = earth_orbit.orbitalPointAfterTime(x.body_state.coord, time_to_stop_msec, x.ccw)
                drawCircle(position_when_stop_moment * scale, earth.radius * scale, GREEN)
                drawCircle(position_when_stop_moment * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
              })
            })
          }
        }
      }
    })
  }
  
  private def ellipseOrbitDataForNonPlayerShip(update_count:Long, 
                                           bs: MutableBodyState, 
                                           body_radius:Double, 
                                           planet_state:MutableBodyState, 
                                           planet:CelestialBody, 
                                           e:EllipseOrbit, 
                                           ccw:Boolean,
                                           orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, e, ccw, () => {
      val new_e = e.withNewFocusPosition(planet_state.coord)
      openglLocalTransform {
        openglMove(new_e.center * scale)
        openglRotateDeg(Vec(-1, 0).signedDeg(new_e.f2 - new_e.f))
        drawEllipse(DVec.zero, new_e.a * scale, new_e.b * scale, orbit_color)
      }
      if(InterfaceHolder.namesSwitcher.showNames) {
        openglLocalTransform {
          openglMove(new_e.orbitalPointByTrueAnomalyDeg(0) * scale)
          drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
          print("P", Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
        }
        openglLocalTransform {
          openglMove(new_e.orbitalPointByTrueAnomalyDeg(180) * scale)
          drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
          print("A", Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
        }
      }
    })
  }
  
  private def ellipseOrbitDataForPlanet(update_count:Long, 
                                        bs: MutableBodyState, 
                                        body_radius:Double, 
                                        planet_state:MutableBodyState, 
                                        planet:CelestialBody, 
                                        e:EllipseOrbit, 
                                        ccw:Boolean,
                                        orbit_color: ScageColor) = {
    OrbitData(update_count, bs, body_radius, planet_state, planet, e, ccw, () => {
      val new_e = e.withNewFocusPosition(planet_state.coord)
      openglLocalTransform {
        openglMove(new_e.center * scale)
        openglRotateDeg(Vec(-1, 0).signedDeg(e.f2 - e.f))
        drawEllipse(DVec.zero, new_e.a * scale, new_e.b * scale, orbit_color)
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
        openglRotateDeg(Vec(-1, 0).signedDeg(new_o.f2 - new_o.f))
        drawEllipse(DVec.zero, new_o.a * scale, new_o.b * scale, orbit_color)
      }
      if(InterfaceHolder.namesSwitcher.showNames) {
        openglLocalTransform {
          openglMove(new_o.orbitalPointByTrueAnomalyDeg(0) * scale)
          drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
          print("P", Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
        }
        openglLocalTransform {
          openglMove(new_o.orbitalPointByTrueAnomalyDeg(180) * scale)
          drawFilledRectCentered(DVec.zero, w/globalScale, w/globalScale, orbit_color)
          print("A", Vec.zero, color = orbit_color, size = (max_font_size / globalScale).toFloat)
        }
      }
      val mouse_point = absCoord(mouseCoord) / scale
      drawLine(new_o.f * scale, mouse_point * scale, DARK_GRAY)

      val orbital_point = new_o.orbitalPointInPoint(mouse_point)
      drawFilledCircle(orbital_point * scale, 3 / globalScale, orbit_color)

      if (_stop_after_number_of_tacts > 0) {
        //drawFilledCircle(new_e.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly) * scale, 3 / globalScale, RED)
        drawFilledCircle(new_o.orbitalPointAfterTime(bs.coord, (_stop_after_number_of_tacts * base_dt * 1000).toLong, ccw) * scale, 3 / globalScale, GREEN)
      }
      val true_anomaly_rad = new_o.tetaRad2PiInPoint(mouse_point)

      lazy val flight_time_msec = new_o.travelTimeOnOrbitMsec(bs.coord, orbital_point, ccw)

      if (set_stop_moment) {
        _stop_after_number_of_tacts = (flight_time_msec / 1000 / base_dt).toLong
        set_stop_moment = false

        /*val p1 = new_e.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly)
        println((50 to 300 by 50).map(num_iterations => {
          val px = new_e.orbitalPointAfterTime(bs.coord, (_stop_after_number_of_tacts*base_dt*1000).toLong, ccw, num_iterations)
          mOrKmOrMKm(p1.dist(px))
        }).mkString(" : "))*/
      }
      if (InterfaceHolder.orbParams.calculationOn) {
        val flight_time_str = s"${timeStr(flight_time_msec)}"
        val (vt, vr) = new_o.orbitalVelocityByTrueAnomalyRad(true_anomaly_rad)
        val vnorm = math.sqrt(vr * vr + vt * vt)
        openglLocalTransform {
          openglMove(orbital_point * scale)
          print(s"  $flight_time_str : ${mOrKmOrMKm(new_o.distanceByTrueAnomalyRad(true_anomaly_rad) - planet.radius)} : ${msecOrKmsec(vnorm)}", Vec.zero, size = (max_font_size / globalScale).toFloat, orbit_color)
        }
        InterfaceHolder.shipInterfaces.filter(si => {
          !si.isMinimized && !si.monitoring_ship.isCrashed && !player_ship.isDockedToShip(si.monitoring_ship)
        }).flatMap(_.monitoring_ship.thisOrActualProxyShipOrbitData).foreach(x => {
          x.ellipseOrbit.foreach(ship_orbit => {
            val position_after_time = ship_orbit.orbitalPointAfterTime(x.body_state.coord, flight_time_msec, x.ccw)
            drawCircle(position_after_time * scale, w/globalScale, YELLOW)
            if (_stop_after_number_of_tacts > 0) {
              val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
              val position_when_stop_moment = ship_orbit.orbitalPointAfterTime(x.body_state.coord, time_to_stop_msec, x.ccw)
              drawCircle(position_when_stop_moment * scale, w/globalScale, GREEN)
            }
          })
        })
        if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == earth.index)) {
          moon.orbitRender.foreach(x => {
            x.ellipseOrbit.foreach(moon_orbit => {
              val position_after_time = moon_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, flight_time_msec)
              drawCircle(position_after_time * scale, moon.radius * scale, YELLOW)
              drawCircle(position_after_time * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
              if (_stop_after_number_of_tacts > 0) {
                val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
                val position_when_stop_moment = moon_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, time_to_stop_msec)
                drawCircle(position_when_stop_moment * scale, moon.radius * scale, GREEN)
                drawCircle(position_when_stop_moment * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
              }
            })
          })
        } else if(player_ship.thisOrActualProxyShipCurrentOrbitData.exists(or => or.planet.index == sun.index)) {
          earth.orbitRender.foreach(x => {
            x.ellipseOrbit.foreach(earth_orbit => {
              val position_after_time = earth_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, flight_time_msec)
              drawCircle(position_after_time * scale, earth.radius * scale, YELLOW)
              drawCircle(position_after_time * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
              if (_stop_after_number_of_tacts > 0) {
                val time_to_stop_msec = (_stop_after_number_of_tacts * base_dt * 1000).toLong
                val position_when_stop_moment = earth_orbit.orbitalPointAfterTimeCCW(x.body_state.coord, time_to_stop_msec)
                drawCircle(position_when_stop_moment * scale, earth.radius * scale, GREEN)
                drawCircle(position_when_stop_moment * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
              }
            })
          })
        }
      }
    })
  }

  def updateOrbitData(update_count:Long,
                      body_index: Int,
                      body_radius:Double,
                      orbit_color: ScageColor,
                      some_system_state: mutable.Map[Int, MutableBodyState],
                      need_planets: Set[Int]): Option[OrbitData] = {
    some_system_state.get(body_index).flatMap(bs => {
      updateOrbitData(update_count, bs, body_radius, orbit_color, some_system_state, need_planets)
    })
  }

  def updateOrbitData(update_count:Long,
                      bs: MutableBodyState,
                      body_radius:Double,
                      orbit_color: ScageColor,
                      some_system_state: mutable.Map[Int, MutableBodyState],
                      need_planets: Set[Int]): Option[OrbitData] = {
    val celestials = some_system_state.filter(kv => need_planets.contains(kv._1)).flatMap(kv => {
      planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
    }).values.toSeq.sortBy(_._2.mass)
    // смотрим, где он находится
    val bs_coord = bs.coord
    val bs_vel = bs.vel
    val bs_mass = bs.mass
    val bs_ang = bs.ang
    insideSphereOfInfluenceOfCelestialBody(bs_coord, bs_mass, celestials) match {
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
