package com.github.dunnololda.scageprojects.orbitalkiller

import java.io.FileOutputStream

import com.github.dunnololda.scage.ScageLibD.{DVec, Vec, addGlyphs, appVersion, max_font_size, messageBounds, print, property, stopApp, _}
import com.github.dunnololda.scageprojects.orbitalkiller.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.components.OrbitalComponents
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.BoxShape
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{BodyState, MutableBodyState}
import com.github.dunnololda.scageprojects.orbitalkiller.util.DrawUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.StringUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.GeometryUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.MathUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.KeplerOrbit._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.{EllipseOrbit, HyperbolaOrbit}
import com.github.dunnololda.scageprojects.orbitalkiller.vessels._

import scala.collection._

//import collection.mutable.ArrayBuffer

object OrbitalKiller extends ScageScreenAppDMT("Orbital Killer", property("screen.width", 1280), property("screen.height", 768)) {
  val components = new OrbitalComponents

  import components._
  import planetComponents._
  import shipComponents._
  import systemEvolutionComponents._

  private var _time_multiplier = realtime

  def timeMultiplier = {
    /*if (_time_multiplier != realtime && ShipsHolder.ships.flatMap(_.engines).exists(_.active)) {
      timeMultiplier_=(realtime)
    }*/
    _time_multiplier
  }

  def timeMultiplier_=(new_time_multiplier: Int) {
    if (new_time_multiplier > 0) {
      _time_multiplier = new_time_multiplier
      ships.flatMap(_.engines).filter(_.active).foreach(e => {
        e.workTimeTacts = e.workTimeTacts
      })
      /*}*/
    }
  }

  /*def maxTimeMultiplier:Int = {
    /*val a = ships.map(s => math.abs(s.currentState.acc*s.currentState.vel.p)).max
    math.max((0.1*math.pow(351.3011068768212/a, 10.0/7)/5).toInt, 1)*/
    1
  }*/

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

  var _set_stop_time: Boolean = false
  var _stop_after_number_of_tacts: Long = 0
  var _stop_in_orbit_true_anomaly: Double = 0

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

  private var view_mode: ViewMode = FreeViewMode

  def viewMode = view_mode

  def viewMode_=(new_view_mode: ViewMode) {
    if (new_view_mode != view_mode) {
      new_view_mode match {
        case FreeViewMode => // свободный
          _center = center
          center = _center
          rotationAngle = 0
          base = DVec.zero
          view_mode = FreeViewMode
        case FixedOnShip => // фиксация на корабле
          center = player_ship.coord + _ship_offset
          base = if (player_ship.coord.norma < 100000) DVec.zero else player_ship.coord
          rotationCenter = player_ship.coord
          rotationAngleDeg = -player_ship.rotation
          view_mode = FixedOnShip
        case Landing => // посадка на планету
          center = player_ship.coord + _ship_offset
          rotationCenter = player_ship.coord
          rotationAngleDeg = {
            val nearest_body_coord = if (player_ship.coord.dist2(earth.coord) < player_ship.coord.dist2(moon.coord)) earth.coord else moon.coord
            val vec = player_ship.coord - nearest_body_coord
            if (vec.x >= 0) vec.deg(DVec(0, 1))
            else vec.deg(DVec(0, 1)) * (-1)
          }
          view_mode = Landing
        case FixedOnShipAbsolute => // фиксация на корабле, абсолютная ориентация
          center = player_ship.coord + _ship_offset
          base = if (player_ship.coord.norma < 100000) DVec.zero else player_ship.coord
          rotationAngle = 0
          view_mode = FixedOnShipAbsolute
        case FixedOnOrbit => // в режиме карты зафиксировать центр орбиты в центре экрана
          if (drawMapMode) {
            _center = _center - orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates)
              .map(_._2.center * scale)
              .getOrElse(player_ship.coord)
            center = orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates)
              .map(_._2.center * scale)
              .getOrElse(player_ship.coord) + _center
            rotationAngle = 0
            view_mode = FixedOnOrbit
          }
        case _ =>
      }
    }
  }

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

  private var disable_interface_drawing = false

  private var _draw_map_mode = false

  def drawMapMode = _draw_map_mode

  def drawMapMode_=(new_mode: Boolean) {
    if (new_mode) {
      _draw_map_mode = true
      orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates) match {
        case Some((planet, kepler_orbit)) =>
          kepler_orbit match {
            case ellipse: EllipseOrbit =>
              val b = BoxShape(2 * ellipse.a, 2 * ellipse.b)
              val aabb = b.aabb(ellipse.center, Vec(-1, 0).signedDeg(ellipse.f2 - ellipse.f))
              viewMode = FreeViewMode
              globalScale = 750 / (aabb.height * scale)
              _center = ellipse.center * scale
              viewMode = FixedOnOrbit
            case hyperbola: HyperbolaOrbit =>
              val b = BoxShape(2 * hyperbola.a, 2 * hyperbola.b)
              val aabb = b.aabb(hyperbola.half_center, Vec(1, 0).signedDeg(hyperbola.f_minus_center_n))
              viewMode = FreeViewMode
              globalScale = 750 / (aabb.height * scale)
              _center = hyperbola.half_center * scale
              viewMode = FixedOnOrbit
          }
        case None =>
          viewMode = FreeViewMode
          globalScale = 1
          _center = earth.coord * scale
          viewMode = FixedOnOrbit
      }
    } else {
      _draw_map_mode = false
      globalScale = 10
      viewMode = FixedOnShip
    }
  }

  private var _show_game_saved_message = false

  def showGameSavedMessage = _show_game_saved_message

  def saveGame() {
    val fos = new FileOutputStream("save.orbitalkiller")
    fos.write(s"time $tacts\n".getBytes)
    currentBodyState(player_ship.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    currentBodyState(station.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    currentBodyState(moon.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    currentBodyState(earth.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    currentBodyState(sun.index).foreach(x => fos.write(s"${x.saveData}\n".getBytes))
    fos.close()
    _show_game_saved_message = true
    val start = System.currentTimeMillis()
    actionStaticPeriodIgnorePause(1000) {
      if (System.currentTimeMillis() - start > 2000) {
        _show_game_saved_message = false
        deleteSelf()
      }
    }
  }

  private var _show_game_loaded_message = false

  def showGameLoadedMessage = _show_game_loaded_message

  private var _show_game_failed_to_load_message = false

  def showGameFailedToLoadMessage = _show_game_failed_to_load_message

  def loadGame() {
    def _parseDVec(str: String): Option[DVec] = {
      val s = str.split(":")
      if (s.length == 2) {
        try {
          Some(DVec(s(0).toDouble, s(1).toDouble))
        } catch {
          case e: Exception => None
        }
      } else None
    }

    def _parseLong(str: String): Option[Long] = {
      try {
        Some(str.toLong)
      } catch {
        case e: Exception => None
      }
    }

    def _parseDouble(str: String): Option[Double] = {
      try {
        Some(str.toDouble)
      } catch {
        case e: Exception => None
      }
    }

    val savefile_lines = io.Source.fromFile("save.orbitalkiller").getLines().toList
    val new_tacts_option = savefile_lines.headOption.flatMap(l => {
      val s = l.split(" ")
      if (s.length == 2) _parseLong(s(1)) else None
    })

    val new_states = (for {
      line <- savefile_lines.drop(1)
      s = line.split(" ")
      if s.length == 7
      index = s(0)
      acc <- _parseDVec(s(1))
      vel <- _parseDVec(s(2))
      coord <- _parseDVec(s(3))
      ang_acc <- _parseDouble(s(4))
      ang_vel <- _parseDouble(s(5))
      ang <- _parseDouble(s(6))
    } yield {
      (index, (acc, vel, coord, ang_acc, ang_vel, ang))
    }).toMap

    for {
      new_tacts <- new_tacts_option
      ship_state <- new_states.get("ship")
      station_state <- new_states.get("station")
      moon_state <- new_states.get("Moon")
      earth_state <- new_states.get("Earth")
      sun_state <- new_states.get("Sun")
    } {
      system_evolution.tacts = new_tacts

      def _loadState(currentState: MutableBodyState, new_state: (DVec, DVec, DVec, Double, Double, Double)): Unit = {
        val (acc, vel, coord, ang_acc, ang_vel, ang) = new_state
        currentState.acc = acc
        currentState.vel = vel
        currentState.coord = coord
        currentState.ang_acc = ang_acc
        currentState.ang_vel = ang_vel
        currentState.ang = ang
      }

      _loadState(player_ship.currentState, ship_state)
      _loadState(station.currentState, station_state)
      _loadState(moon.currentState, moon_state)
      _loadState(earth.currentState, earth_state)
      _loadState(sun.currentState, sun_state)
      _show_game_loaded_message = true
    }
    if (_show_game_loaded_message) {
      val start = System.currentTimeMillis()
      actionStaticPeriodIgnorePause(1000) {
        if (System.currentTimeMillis() - start > 2000) {
          _show_game_loaded_message = false
          deleteSelf()
        }
      }
    } else {
      _show_game_failed_to_load_message = true
      val start = System.currentTimeMillis()
      actionStaticPeriodIgnorePause(1000) {
        if (System.currentTimeMillis() - start > 2000) {
          _show_game_failed_to_load_message = false
          deleteSelf()
        }
      }
    }
  }

  val ccw_symbol = '\u21b6'
  val cw_symbol = '\u21b7'
  val rocket_symbol = '\u2191'

  preinit {
    addGlyphs(s"$ccw_symbol$cw_symbol$rocket_symbol")
  }

  keyIgnorePause(KEY_RETURN, onKeyDown = {
    if (player_ship.isAlive) player_ship.launchRocket()
  })

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD1)
  })
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD2)
  })
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD3)
  })
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD4)
  })
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD6)
  })
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD7)
  })
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD8)
  })
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {
    if (player_ship.isAlive) player_ship.selectOrSwitchEngineActive(KEY_NUMPAD9)
  })

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {
    if (player_ship.isAlive) {
      player_ship.engines.foreach(e => {
        if (e.active || e.power > 0) {
          e.active = false
          e.power = 0
        } else {
          e.workTimeTacts = 0
        }
      })
      player_ship.clearEngineSelection()
    }
  })

  /*keyIgnorePause(KEY_NUMPAD0, onKeyDown = {
    ship.engines.filterNot(_.active).foreach(e => e.workTimeTacts = 226800)     // 1 hour
    val active_engines = ship.engines.filter(_.active)
    if(active_engines.map(ae => ae.fuelConsumptionPerTact*226800).sum <= ship.fuelMass) {
      active_engines.foreach(e => e.workTimeTacts = 226800)
    } else {
      val fuel_for_every_active_engine = ship.fuelMass / active_engines.length
      active_engines.foreach(e => e.workTimeTacts = (fuel_for_every_active_engine/e.fuelConsumptionPerTact).toLong)
    }
  })*/


  private def repeatTime(code: Int): Long = {
    keyPress(code).map { kp =>
      if (kp.was_pressed && System.currentTimeMillis() - kp.pressed_start_time < 100) 100l
      else 10l
    }.getOrElse(100l)
  }

  keyIgnorePause(KEY_UP, repeatTime(KEY_UP), onKeyDown = {
    if (player_ship.isAlive) {
      if (player_ship.flightMode != NearestPlanetVelocity) {
        player_ship.selectedEngine.foreach(e => e.powerPercent += 1)
      } else {
        player_ship.vertical_speed_msec += 1
      }
    }
  }, onKeyUp = if (player_ship.isAlive && player_ship.flightMode != NearestPlanetVelocity && player_ship.selectedEngine.exists(_.active)) {
    needToUpdateOrbits("KEY_UP")
  })
  keyIgnorePause(KEY_DOWN, repeatTime(KEY_DOWN), onKeyDown = {
    if (player_ship.isAlive) {
      if (player_ship.flightMode != NearestPlanetVelocity) {
        player_ship.selectedEngine.foreach(e => e.powerPercent -= 1)
      } else {
        player_ship.vertical_speed_msec -= 1
      }
    }
  }, onKeyUp = if (player_ship.isAlive && player_ship.flightMode != NearestPlanetVelocity && player_ship.selectedEngine.exists(_.active)) {
    needToUpdateOrbits("KEY_DOWN")
  })
  keyIgnorePause(KEY_T, onKeyDown = _set_stop_time = !_set_stop_time)
  keyIgnorePause(KEY_RIGHT, repeatTime(KEY_RIGHT), onKeyDown = {
    if (_set_stop_time) {
      _stop_after_number_of_tacts += InterfaceHolder.timeStepSwitcher.timeStep
    } else {
      if (player_ship.isAlive) {
        if (player_ship.flightMode != NearestPlanetVelocity) {
          player_ship.selectedEngine.foreach(e => {
            e.workTimeTacts += InterfaceHolder.timeStepSwitcher.timeStep
            //updateFutureTrajectory()
          })
        } else {
          player_ship.horizontal_speed_msec -= 1
        }
      }
    }
  }, onKeyUp = if (!_set_stop_time && player_ship.isAlive && player_ship.flightMode != NearestPlanetVelocity && player_ship.selectedEngine.exists(_.active)) {
    needToUpdateOrbits("KEY_RIGHT")
  })
  keyIgnorePause(KEY_LEFT, repeatTime(KEY_LEFT), onKeyDown = {
    if (_set_stop_time) {
      _stop_after_number_of_tacts -= InterfaceHolder.timeStepSwitcher.timeStep
    } else {
      if (player_ship.isAlive) {
        if (player_ship.flightMode != NearestPlanetVelocity) {
          player_ship.selectedEngine.foreach(e => {
            /*if(e.worktimeTacts > 0) {*/
            e.workTimeTacts -= InterfaceHolder.timeStepSwitcher.timeStep
            //updateFutureTrajectory()
            /*}*/
          })
        } else {
          player_ship.horizontal_speed_msec += 1
        }
      }
    }
  }, onKeyUp = {
    if (!_set_stop_time && player_ship.isAlive && player_ship.flightMode != NearestPlanetVelocity && player_ship.selectedEngine.exists(_.active)) {
      needToUpdateOrbits("KEY_LEFT")
    }
  })

  private val engine_keys = List(KEY_UP, KEY_DOWN, KEY_RIGHT, KEY_LEFT)

  def anyEngineKeyPressed = engine_keys.exists(k => keyPressed(k))

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    timeMultiplier += realtime
  } /*, onKeyUp = updateFutureTrajectory("KEY_ADD")*/)
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    if (timeMultiplier > realtime) {
      timeMultiplier -= realtime
    }
  } /*, onKeyUp = updateFutureTrajectory("KEY_SUBTRACT")*/)

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    if (timeMultiplier == realtime) {
      timeMultiplier = realtime * 50
    } else {
      timeMultiplier += realtime * 50
    }
  } /*, onKeyUp = updateFutureTrajectory("KEY_MULTIPLY")*/)
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    if (timeMultiplier != realtime) {
      timeMultiplier = realtime
    }
  } /*, onKeyUp = updateFutureTrajectory("KEY_DIVIDE")*/)

  keyIgnorePause(KEY_W, 10, onKeyDown = {
    if (drawMapMode) _center += DVec(0, 5 / globalScale) else _ship_offset += DVec(0, 5 / globalScale)
  })
  keyIgnorePause(KEY_A, 10, onKeyDown = {
    if (drawMapMode) _center += DVec(-5 / globalScale, 0) else _ship_offset += DVec(-5 / globalScale, 0)
  })
  keyIgnorePause(KEY_S, 10, onKeyDown = {
    if (drawMapMode) _center += DVec(0, -5 / globalScale) else _ship_offset += DVec(0, -5 / globalScale)
  })
  keyIgnorePause(KEY_D, 10, onKeyDown = {
    if (drawMapMode) _center += DVec(5 / globalScale, 0) else _ship_offset += DVec(5 / globalScale, 0)
  })

  keyIgnorePause(KEY_M, onKeyDown = {
    drawMapMode = !drawMapMode
  })

  keyIgnorePause(KEY_SPACE, onKeyDown = {
    if (!drawMapMode) {
      _ship_offset = DVec.zero
    } else {
      drawMapMode = true
    }
  })

  keyIgnorePause(KEY_1, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = FreeFlightMode)
  keyIgnorePause(KEY_2, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = Killrot)
  keyIgnorePause(KEY_3, onKeyDown = if (player_ship.isAlive) {
    if (keyPressed(KEY_LSHIFT) || keyPressed(KEY_RSHIFT)) player_ship.flightMode = OppositeRelativeVelocityAligned else player_ship.flightMode = RelativeVelocityAligned
  })
  keyIgnorePause(KEY_4, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = CirclularOrbit)
  keyIgnorePause(KEY_5, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = NearestShipVelocity)
  keyIgnorePause(KEY_6, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = NearestShipAligned)
  keyIgnorePause(KEY_7, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = NearestShipAutoDocking)
  keyIgnorePause(KEY_8, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = NearestPlanetVelocity)
  // KEY_9 - vacant
  keyIgnorePause(KEY_0, onKeyDown = if (player_ship.isAlive) player_ship.flightMode = Maneuvering)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {
    pause()
    holdCounters {
      HelpScreen.run()
    }
  })
  keyIgnorePause(KEY_F2, onKeyDown = if (!drawMapMode) viewMode = FixedOnShip else viewMode = FreeViewMode) // фиксация на корабле, в режиме карты: свободный режим
  keyIgnorePause(KEY_F3, onKeyDown = if (!drawMapMode) viewMode = FixedOnShipAbsolute else viewMode = FixedOnOrbit) // фиксация на корабле, абсолютная ориентация, в режиме карты: фиксация на орбите
  keyIgnorePause(KEY_F4, onKeyDown = if (!drawMapMode) viewMode = Landing) // посадка на планету, если не в режиме карты

  // функционал толком не работает
  keyIgnorePause(KEY_F5, onKeyDown = saveGame()) // сохранить текущее состояние системы
  //keyIgnorePause(KEY_F6, onKeyDown = loadGame())                          // загрузить из файла состояние системы

  keyIgnorePause(KEY_I, onKeyDown = {
    disable_interface_drawing = !disable_interface_drawing
    if (disable_interface_drawing) InterfaceHolder.hideAllByUser()
    else InterfaceHolder.showAllByUser()
  })
  keyIgnorePause(KEY_R, onKeyDown = {
    if (!player_ship.isDocked) {
      player_ship.rockets_enabled = !player_ship.rockets_enabled
      if (player_ship.rockets_enabled) {
        if (InterfaceHolder.rocketsInfo.isMinimizedByUser) {
          InterfaceHolder.rocketsInfo.showByUser()
        }
      }
    }
  })
  keyIgnorePause(KEY_Q, onKeyDown = {
    if (keyPressed(KEY_LCONTROL)) stopApp()
  })

  keyIgnorePause(KEY_N, 100, onKeyDown = {
    if (InterfaceHolder.realTrajectorySwitcher.showRealTrajectory) {
      InterfaceHolder.realTrajectorySwitcher.numPoints += 24 * 3600
    }
  })

  keyIgnorePause(KEY_C, onKeyDown = {
    if (InterfaceHolder.realTrajectorySwitcher.showRealTrajectory) {
      if (realTrajectory.curPoints < InterfaceHolder.realTrajectorySwitcher.numPoints) {
        InterfaceHolder.realTrajectorySwitcher.numPoints = realTrajectory.curPoints
      } else {
        InterfaceHolder.realTrajectorySwitcher.numPoints = 24 * 3600
        needToUpdateOrbits("reset real trajectory num points")
      }
    }
  })

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if (globalScale > 0.01) {
      if (globalScale.toInt >= 200000) globalScale -= 100000
      else if (globalScale.toInt >= 20000) globalScale -= 10000
      else if (globalScale.toInt >= 2000) globalScale -= 1000
      else if (globalScale.toInt >= 200) globalScale -= 100
      else if (globalScale.toInt >= 20) globalScale -= 10
      else if (globalScale.toInt >= 2) globalScale -= 1
      else if ((globalScale * 10).toInt > 1) globalScale -= 0.1
      else globalScale -= 0.01
      if (globalScale < 0.01) globalScale = 0.01
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    val _maxGlobalScale = if (!drawMapMode) 30 else 1000000
    if (globalScale < _maxGlobalScale) {
      if (globalScale < 0.1) globalScale += 0.01
      else if (globalScale < 1) globalScale += 0.1
      else if (globalScale < 10) globalScale += 1
      else if (globalScale < 100) globalScale += 10
      else if (globalScale < 1000) globalScale += 100
      else if (globalScale < 10000) globalScale += 1000
      else if (globalScale < 100000) globalScale += 10000
      else globalScale += 100000
      if (globalScale > _maxGlobalScale) globalScale = _maxGlobalScale
    }
    println(globalScale)
  })


  private var left_up_corner: Option[DVec] = None
  private var right_down_corner: Option[DVec] = None
  var set_stop_moment = false
  leftMouseIgnorePause(onBtnDown = m => {
    if (drawMapMode) {
      left_up_corner = Some(absCoord(m))
    } else {
      InterfaceHolder.clickInterfaceElem(m, 0)
    }
  }, onBtnUp = m => {
    if (drawMapMode) {
      if (right_down_corner.nonEmpty) {
        for {
          x <- left_up_corner
          y <- right_down_corner
          c = (y - x).n * (y.dist(x) / 2f) + x
          h = math.abs(y.y - x.y)
          if h * globalScale > 10
        } {
          globalScale = math.min(1000000, 750 / h)
          if (viewMode == FixedOnOrbit) {
            _center = c - orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates)
              .map(_._2.center * scale).getOrElse(player_ship.coord)
          } else {
            _center = c
          }
        }
      } else {
        if (!InterfaceHolder.clickInterfaceElem(m, 0) && player_ship.isAlive && (keyPressed(KEY_LSHIFT) || keyPressed(KEY_RSHIFT))) {
          set_stop_moment = true
        }
      }
      left_up_corner = None
      right_down_corner = None
    }
  })
  leftMouseDragIgnorePause(onDrag = m => if (drawMapMode) {
    right_down_corner = Some(absCoord(m))
  })

  rightMouseIgnorePause(onBtnDown = m => {
    if (!InterfaceHolder.clickInterfaceElem(m, 1) && (keyPressed(KEY_LSHIFT) || keyPressed(KEY_RSHIFT))) {
      _stop_after_number_of_tacts = 0
    }
  })

  action {
    nextStep()
  }

  private var _center = player_ship.coord
  private var _ship_offset = DVec.zero

  def shipOffset = _ship_offset

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
        case OrbitData(_, bs, _, planet_state, _, _, _, render) =>
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
        case OrbitData(_, bs, _, planet_state, _, _, _, render) =>
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
        case or@OrbitData(_, bs, _, planet_state, _, _, _, render) =>
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
            case or@OrbitData(_, bs, _, planet_state, _, _, _, render) =>
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






