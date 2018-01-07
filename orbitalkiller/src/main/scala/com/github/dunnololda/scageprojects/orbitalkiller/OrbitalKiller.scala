package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD.{DVec, Vec, addGlyphs, appVersion, max_font_size, messageBounds, print, property, _}
import com.github.dunnololda.scageprojects.orbitalkiller.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.components.OrbitalComponents
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{BodyState, MutableBodyState}
import com.github.dunnololda.scageprojects.orbitalkiller.render.OrbitRenderData
import com.github.dunnololda.scageprojects.orbitalkiller.util.DrawUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.StringUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.GeometryUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.MathUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.KeplerOrbit._
import com.github.dunnololda.scageprojects.orbitalkiller.vessels._

import scala.collection._

//import collection.mutable.ArrayBuffer

object OrbitalKiller
  extends ScageScreenAppDMT("Orbital Killer", property("screen.width", 1280), property("screen.height", 768))
    with OrbitKeyControls with OrbitMouseControls {
  val orbitalComponents: OrbitalComponents = new OrbitalComponents

  import orbitalComponents._
  import planetComponents._
  import shipComponents._
  import systemEvolutionComponents._

  def needToUpdateOrbits(reason: String) {
    println(s"needToUpdateOrbits: $reason")
    if (onPause) {
      system_cache.clear()
      _update_orbits = true
      realTrajectory.init()
      //RealTrajectory2.init()
      //RealTrajectory3.init()
    }
  }

  actionDynamicPeriodIgnorePause(500 / timeMultiplier) {
    realTrajectory.continue()
    //RealTrajectory2.continue()
    //RealTrajectory3.continue()
  }

  private def nextStep() {
    (1 to timeMultiplier).foreach(step => {
      ships.foreach(s => {
        s.beforeStep()
      })
      system_evolution.step()
      ships.foreach(s => {
        s.afterStep(timeMsec)
      })
      if (_stop_after_number_of_tacts > 0) {
        _stop_after_number_of_tacts -= 1
        if (_stop_after_number_of_tacts <= 0) {
          if (timeMultiplier != realtime) {
            timeMultiplier = realtime
          }
          pause()
        }
      }
    })
  }

  nextStep()

  /*def satelliteSpeedStrInPoint(coord: DVec, velocity: DVec, mass: Double): String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val ss = satelliteSpeed(coord, velocity, planet_state.coord, planet_state.vel, planet_state.mass, G)
        f"${msecOrKmsec(ss.norma)} (velx = ${msecOrKmsec(ss.x)}, vely = ${msecOrKmsec(ss.y)})"
      case None =>
        "N/A"
    }
  }*/

  /*def escapeVelocityStrInPoint(coord: DVec, velocity: DVec, mass: Double): String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val ss = escapeVelocity(coord, velocity, planet_state.coord, planet_state.vel, planet_state.mass, G)
        f"${msecOrKmsec(ss.norma)} (velx = ${msecOrKmsec(ss.x)}, vely = ${msecOrKmsec(ss.y)})"
      case None =>
        "N/A"
    }
  }*/

  def curvatureRadiusStrInPoint(body_state: BodyState): String = {
    s"${mOrKmOrMKm(curvatureRadiusInPoint(body_state))}"
  }

  def orbitStrInPointWithVelocity(coord: DVec, velocity: DVec, radius: Double, mass: Double, planet_states: Seq[(CelestialBody, MutableBodyState)]): String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, planet_states) match {
      case Some((planet, planet_state)) =>
        val orbit = calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)
        orbit.strDefinition(planet.name, planet.radius, planet_state.vel, planet_state.ang, planet.groundSpeedMsec, planet.g, coord, velocity, radius)
      case None => "N/A"
    }
  }

  /*def orbitInPointWithVelocity(coord: DVec, velocity: DVec, mass: Double): Option[KeplerOrbit] = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        Some(calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G))
      case None => None
    }
  }*/

  val ccw_symbol = '\u21b6'
  val cw_symbol = '\u21b7'
  val rocket_symbol = '\u2191'

  preinit {
    addGlyphs(s"$ccw_symbol$cw_symbol$rocket_symbol")
  }

  action {
    nextStep()
  }

  center = _center
  windowCenter = DVec((windowWidth - 1024) + 1024 / 2, windowHeight / 2)
  viewMode = FixedOnShip
  globalScale = 10

  private var _update_orbits = false
  private var update_count: Long = 0l

  private def updateOrbits() {
    //println("updateOrbits")
    if (player_ship.flightMode == Maneuvering || !onPause || !player_ship.engines.exists(_.active)) {
      // если в режиме маневрирования, или не в режиме маневрирования, но не на паузе, или на паузе, но двигатели не работают - рисуем текущее состояние
      moon.orbitRender = orbitDataUpdater.updateOrbitData(update_count, moon.index, moon.radius, player_ship.colorIfPlayerAliveOrRed(GREEN), system_evolution.allBodyStates, Set(earth.index, sun.index), None)
      earth.orbitRender = orbitDataUpdater.updateOrbitData(update_count, earth.index, earth.radius, player_ship.colorIfPlayerAliveOrRed(ORANGE), system_evolution.allBodyStates, Set(sun.index), None)
      player_ship.updateOrbitData(update_count, player_ship.colorIfPlayerAliveOrRed(YELLOW), timeMsec, system_evolution.allBodyStates, InterfaceHolder.orbitSwitcher.calculateOrbitAround)
      InterfaceHolder.orbitInfo.markUpdateNeeded()
      InterfaceHolder.shipInterfaces.foreach(si => {
        if (!si.isMinimized && !si.monitoring_ship.isCrashed) {
          si.monitoring_ship.updateOrbitData(update_count, player_ship.colorIfPlayerAliveOrRed(MAGENTA), timeMsec, system_evolution.allBodyStates)
          si.markUpdateNeeded()
        }
      })
    } else {
      // в эту секцию мы попадаем, если мы не в режиме маневрирования, на паузе, и двигатели работают
      val stop_moment_tacts = player_ship.engines.map(_.stopMomentTacts).max
      val system_state_when_engines_off = getFutureState(stop_moment_tacts)
      moon.orbitRender = orbitDataUpdater.updateOrbitData(update_count, moon.index, moon.radius, player_ship.colorIfPlayerAliveOrRed(GREEN), system_state_when_engines_off, Set(earth.index, sun.index), None)
      earth.orbitRender = orbitDataUpdater.updateOrbitData(update_count, earth.index, earth.radius, player_ship.colorIfPlayerAliveOrRed(ORANGE), system_state_when_engines_off, Set(sun.index), None)
      val stop_moment_msec = (stop_moment_tacts * base_dt * 1000).toLong
      player_ship.updateOrbitData(update_count, player_ship.colorIfPlayerAliveOrRed(YELLOW), stop_moment_msec, system_state_when_engines_off, InterfaceHolder.orbitSwitcher.calculateOrbitAround)
      InterfaceHolder.orbitInfo.markUpdateNeeded()
      InterfaceHolder.shipInterfaces.foreach(si => {
        if (!si.isMinimized && !si.monitoring_ship.isCrashed) {
          si.monitoring_ship.updateOrbitData(update_count, player_ship.colorIfPlayerAliveOrRed(MAGENTA), stop_moment_msec, system_state_when_engines_off, None)
          si.markUpdateNeeded()
        }
      })
    }
    update_count += 1
    _update_orbits = false
  }

  updateOrbits()

  actionDynamicPeriodIgnorePause(1000 / timeMultiplier) {
    if ( /*drawMapMode && (*/ !onPause || _update_orbits /*)*/ ) {
      updateOrbits()
    }
  }

  actionStaticPeriodIgnorePause(10000) {
    if (timeMultiplier != realtime && timeMultiplier > 1f * timeMultiplier / 63 * ticks + 20) {
      println("updating timeMultiplier")
      timeMultiplier = (timeMultiplier * 1f / 63 * ticks).toInt
    }
  }

  def inShadowOfPlanet(coord: DVec): Option[(CelestialBody, MutableBodyState)] = {
    val ship_sun_dist = coord.dist(sun.coord)
    currentPlanetStates.filterNot(_._1.index == sun.index).find {
      case (planet, planet_state) =>
        ship_sun_dist > planet.coord.dist(sun.coord) && (tangentsFromCircleToCircle(planet.coord, planet.radius, sun.coord, sun.radius) match {
          case Some((c1, c2, b1, b2)) =>
            val a1 = (c1 - b1).perpendicular * (coord - b1) > 0
            val a2 = (c2 - b2).perpendicular * (coord - b2) < 0
            a1 && a2
          case None => false
        })
    }
  }

  render {
    if (drawMapMode) {
      /*val spaces = splitSpace(new Space(our_mutable_system.map(_._1), DVec(OrbitalKiller.earth.radius*2, -OrbitalKiller.earth.radius*2)), 5, 2)
      spaces.foreach(s => {
        drawRectCentered(s.center*scale, s.width*scale, s.height*scale, GRAY)
      })
      println(spaces.filter(_.bodies.length > 1).map(x => s"${x.bodies.length}").mkString(" : "))*/

      drawCircle(sun.coord * scale, sun.radius * scale, WHITE)
      val ww = earth.radius * scale / 2f / globalScale

      earth.orbitRender.foreach {
        case OrbitRenderData(_, bs, _, planet_state, _, _, _, render) =>
          drawCircle(bs.coord * scale, earth.radius * scale, WHITE)
          if (InterfaceHolder.namesSwitcher.showNames) {
            openglLocalTransform {
              openglMove(bs.coord.toVec * scale)
              print(earth.name, Vec.zero, color = WHITE, size = (max_font_size / globalScale).toFloat)
            }
            val v = (planet_state.coord - bs.coord).n * earth_sun_eq_gravity_radius

            openglLocalTransform {
              openglMove((bs.coord + v).toVec * scale)
              drawFilledCircle(Vec.zero, ww, DARK_GRAY)
              print(" L1", Vec.zero, color = DARK_GRAY, size = (max_font_size / globalScale).toFloat)
            }
            openglLocalTransform {
              openglMove((bs.coord - v).toVec * scale)
              drawFilledCircle(Vec.zero, ww, DARK_GRAY)
              print(" L2", Vec.zero, color = DARK_GRAY, size = (max_font_size / globalScale).toFloat)
            }
          }

          drawCircle(bs.coord * scale, earth.half_hill_radius * scale, color = DARK_GRAY)
          drawLine(bs.coord * scale, bs.coord * scale + DVec(0, earth.radius * scale).rotateDeg(bs.ang), WHITE)
          drawSunTangents(bs.coord, earth.radius, planet_state.coord, sun.radius, 500000000)
          render()
      }

      moon.orbitRender.foreach {
        case OrbitRenderData(_, bs, _, planet_state, _, _, _, render) =>
          drawCircle(bs.coord * scale, moon.radius * scale, WHITE)
          if (InterfaceHolder.namesSwitcher.showNames) {
            openglLocalTransform {
              openglMove(bs.coord.toVec * scale)
              print(moon.name, Vec.zero, color = WHITE, size = (max_font_size / globalScale).toFloat)
            }
            val v = (planet_state.coord - bs.coord).n * moon_earth_eq_gravity_radius
            openglLocalTransform {
              openglMove((bs.coord + v).toVec * scale)
              drawFilledCircle(Vec.zero, ww, DARK_GRAY)
              print(" L1", Vec.zero, color = DARK_GRAY, size = (max_font_size / globalScale).toFloat)
            }
            openglLocalTransform {
              openglMove((bs.coord - v).toVec * scale)
              drawFilledCircle(Vec.zero, ww, DARK_GRAY)
              print(" L2", Vec.zero, color = DARK_GRAY, size = (max_font_size / globalScale).toFloat)
            }
          }
          //drawCircle(moon.coord*scale, equalGravityRadius(moon.currentState, earth.currentState)*scale, color = DARK_GRAY)
          drawCircle(bs.coord * scale, moon.half_hill_radius * scale, color = DARK_GRAY)
          //drawCircle(moon.coord*scale, soi(moon.mass, earth.coord.dist(moon.coord), earth.mass)*scale, color = DARK_GRAY)
          drawLine(bs.coord * scale, bs.coord * scale + DVec(0, moon.radius * scale).rotateDeg(bs.ang), WHITE)
          drawSunTangents(bs.coord, moon.radius, sun.coord, sun.radius, 40000000)
          render()
      }

      player_ship.thisOrActualProxyShipOrbitData.foreach {
        case or@OrbitRenderData(_, bs, _, planet_state, _, _, _, render) =>
          drawFilledCircle(bs.coord * scale, earth.radius * scale / 2f / globalScale, WHITE)
          if (InterfaceHolder.namesSwitcher.showNames) {
            openglLocalTransform {
              openglMove(bs.coord.toVec * scale)
              print(player_ship.name, Vec.zero, color = WHITE, size = (max_font_size / globalScale).toFloat)
            }
          }
          if (!or.is_landed) render()
      }

      InterfaceHolder.shipInterfaces.foreach(si => {
        if (!si.isMinimized) {
          si.monitoring_ship.thisOrActualProxyShipOrbitData.foreach {
            case or@OrbitRenderData(_, bs, _, planet_state, _, _, _, render) =>
              val color = if (player_ship.isDead || si.monitoring_ship.isDead) RED else MAGENTA
              drawFilledCircle(bs.coord * scale, earth.radius * scale / 2f / globalScale, color)
              if (InterfaceHolder.namesSwitcher.showNames) {
                openglLocalTransform {
                  openglMove(bs.coord.toVec * scale)
                  print(si.monitoring_ship.name, Vec.zero, color = color, size = (max_font_size / globalScale).toFloat)
                }
              }
              if (!si.monitoring_ship.isCrashed && !or.is_landed) render()
          }
        }
      })

      for {
        x <- left_up_corner
        y <- right_down_corner
      } {
        val c = (y - x).n * (y.dist(x) / 2f) + x
        val w = math.abs(y.x - x.x)
        val h = math.abs(y.y - x.y)

        drawRectCentered(c, w, h, DARK_GRAY)
        drawLine(c + DVec(-w / 2, -h / 2), c + DVec(-w / 2, -h / 2) + DVec(0, -10 / globalScale))
        drawLine(c + DVec(w / 2, h / 2), c + DVec(w / 2, h / 2) + DVec(0, -10 / globalScale))

        drawArrow(c + DVec(0, -h / 2) + DVec(0, -5 / globalScale), c + DVec(-w / 2, -h / 2) + DVec(0, -5 / globalScale), DARK_GRAY)
        drawArrow(c + DVec(0, -h / 2) + DVec(0, -5 / globalScale), c + DVec(w / 2, -h / 2) + DVec(0, -5 / globalScale), DARK_GRAY)

        openglLocalTransform {
          val k = messageBounds(s"${mOrKmOrMKm((w / scale).toInt)}", (max_font_size / globalScale).toFloat)
          openglMove((c + DVec(0, -h / 2) + DVec(0, -15 / globalScale) + DVec(-k.x / 2, -k.y / 2)).toVec)
          print(
            s"${mOrKmOrMKm((w / scale).toInt)}",
            Vec.zero,
            color = DARK_GRAY,
            size = (max_font_size / globalScale).toFloat
          )
        }

        drawLine(c + DVec(w / 2, h / 2), c + DVec(w / 2, h / 2) + DVec(10 / globalScale, 0))
        drawLine(c + DVec(w / 2, -h / 2), c + DVec(w / 2, -h / 2) + DVec(10 / globalScale, 0))

        drawArrow(c + DVec(w / 2, 0) + DVec(5 / globalScale, 0), c + DVec(w / 2, h / 2) + DVec(5 / globalScale, 0), DARK_GRAY)
        drawArrow(c + DVec(w / 2, 0) + DVec(5 / globalScale, 0), c + DVec(w / 2, -h / 2) + DVec(5 / globalScale, 0), DARK_GRAY)

        openglLocalTransform {
          val l = messageBounds(s"${mOrKmOrMKm((h / scale).toInt)}", (max_font_size / globalScale).toFloat)
          openglMove((c + DVec(w / 2, 0) + DVec(10 / globalScale, 0) + DVec(0, -l.y / 2)).toVec)
          print(
            s"${mOrKmOrMKm((h / scale).toInt)}",
            Vec.zero,
            color = DARK_GRAY,
            size = (max_font_size / globalScale).toFloat
          )
        }
      }

      if (left_up_corner.isEmpty) {
        val m = absCoord(mouseCoord)
        val d = (player_ship.coord * scale).dist(m) / scale
        drawArrow(player_ship.coord * scale, m, DARK_GRAY)
        openglLocalTransform {
          openglMove(m)
          print(s"  ${mOrKmOrMKm(d.toLong)}", Vec.zero, size = (max_font_size / globalScale).toFloat, DARK_GRAY)
        }
      }
    } else {
      val m = absCoord(mouseCoord)
      val d = player_ship.coord.dist(m)
      openglLocalTransform {
        openglMove(player_ship.coord - base)
        drawArrow(DVec.zero, m - player_ship.coord, DARK_GRAY)
        openglMove(m - player_ship.coord)
        openglRotateDeg(-rotationAngleDeg)
        print(s"  ${mOrKmOrMKm(d.toLong)}", Vec.zero, size = (max_font_size / globalScale).toFloat, DARK_GRAY)
      }
    }
    /*}*/
  }

  interface {
    if (onPause) print("Пауза", windowCenter.toVec, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$ticks", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec * fps / (averageRenderTimeMsec * fps + averageActionTimeMsec * ticks) * 100}%.2f%%/${1 * averageActionTimeMsec * ticks / (averageRenderTimeMsec * fps + averageActionTimeMsec * ticks) * 100}%.2f%%", windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec", windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec", windowWidth - 20, windowHeight - 100, align = "top-right", color = DARK_GRAY)

    val a = DVec(windowWidth - 250, 20)
    val b = DVec(windowWidth - 250 + 100, 20)
    drawLine(a, b, DARK_GRAY)
    drawLine(a, a + (a - b).rotateDeg(90).n * 5, DARK_GRAY)
    drawLine(b, b + (a - b).rotateDeg(90).n * 5, DARK_GRAY)
    print(s"${mOrKmOrMKm((100 / globalScale / (if (drawMapMode) scale else 1.0)).toInt)}", b.toVec, DARK_GRAY)

    InterfaceHolder.update()
    InterfaceHolder.draw()
  }

  pause()
}






