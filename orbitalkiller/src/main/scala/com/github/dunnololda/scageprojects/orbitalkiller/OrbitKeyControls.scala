package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.components._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.vessels._

/**
  * Created by andrey on 1/7/18.
  */
trait OrbitKeyControls extends OrbitalComponentsAware {
  this: ScageScreenAppDMT =>

  private val player_ship: Ship4 = orbitalComponents.shipComponents.player_ship
  var _set_stop_time: Boolean = false

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
    orbitalComponents.orbitsUpdater.scheduleOrbitsUpdate("KEY_UP")
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
    orbitalComponents.orbitsUpdater.scheduleOrbitsUpdate("KEY_DOWN")
  })
  keyIgnorePause(KEY_T, onKeyDown = _set_stop_time = !_set_stop_time)
  keyIgnorePause(KEY_RIGHT, repeatTime(KEY_RIGHT), onKeyDown = {
    if (_set_stop_time) {
      orbitalComponents.stop_after_number_of_tacts.inc(InterfaceHolder.timeStepSwitcher.timeStep)
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
    orbitalComponents.orbitsUpdater.scheduleOrbitsUpdate("KEY_RIGHT")
  })
  keyIgnorePause(KEY_LEFT, repeatTime(KEY_LEFT), onKeyDown = {
    if (_set_stop_time) {
      orbitalComponents.stop_after_number_of_tacts.dec(InterfaceHolder.timeStepSwitcher.timeStep)
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
      orbitalComponents.orbitsUpdater.scheduleOrbitsUpdate("KEY_LEFT")
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

  private val engine_keys = List(KEY_UP, KEY_DOWN, KEY_RIGHT, KEY_LEFT)

  def anyEngineKeyPressed: Boolean = engine_keys.exists(k => keyPressed(k))

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    orbitalComponents.timeMultiplier.incByRealTime()
  } /*, onKeyUp = updateFutureTrajectory("KEY_ADD")*/)
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    orbitalComponents.timeMultiplier.decByRealTime()
  } /*, onKeyUp = updateFutureTrajectory("KEY_SUBTRACT")*/)

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    if (orbitalComponents.timeMultiplier.isRealTime) {
      orbitalComponents.timeMultiplier.setToRealTime(50)
    } else {
      orbitalComponents.timeMultiplier.incByRealTime(50)
    }
  } /*, onKeyUp = updateFutureTrajectory("KEY_MULTIPLY")*/)
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    orbitalComponents.timeMultiplier.setToRealTime()
  } /*, onKeyUp = updateFutureTrajectory("KEY_DIVIDE")*/)

  keyIgnorePause(KEY_W, 10, onKeyDown = {
    if (orbitalComponents.drawMapMode) {
      orbitalComponents._center += DVec(0, 5 / globalScale)
    } else {
      orbitalComponents._ship_offset += DVec(0, 5 / globalScale)
    }
  })
  keyIgnorePause(KEY_A, 10, onKeyDown = {
    if (orbitalComponents.drawMapMode) {
      orbitalComponents._center += DVec(-5 / globalScale, 0)
    } else {
      orbitalComponents._ship_offset += DVec(-5 / globalScale, 0)
    }
  })
  keyIgnorePause(KEY_S, 10, onKeyDown = {
    if (orbitalComponents.drawMapMode) {
      orbitalComponents._center += DVec(0, -5 / globalScale)
    } else {
      orbitalComponents._ship_offset += DVec(0, -5 / globalScale)
    }
  })
  keyIgnorePause(KEY_D, 10, onKeyDown = {
    if (orbitalComponents.drawMapMode) {
      orbitalComponents._center += DVec(5 / globalScale, 0)
    } else {
      orbitalComponents._ship_offset += DVec(5 / globalScale, 0)
    }
  })

  keyIgnorePause(KEY_M, onKeyDown = {
    orbitalComponents.drawMapMode = !orbitalComponents.drawMapMode
  })

  keyIgnorePause(KEY_SPACE, onKeyDown = {
    if (!orbitalComponents.drawMapMode) {
      orbitalComponents._ship_offset = DVec.zero
    } else {
      orbitalComponents.drawMapMode = true
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
  keyIgnorePause(KEY_F2, onKeyDown = if (!orbitalComponents.drawMapMode) {
    orbitalComponents.viewMode = FixedOnShip
  } else {
    orbitalComponents.viewMode = FreeViewMode
  }) // фиксация на корабле, в режиме карты: свободный режим
  keyIgnorePause(KEY_F3, onKeyDown = if (!orbitalComponents.drawMapMode) {
    orbitalComponents.viewMode = FixedOnShipAbsolute
  } else {
    orbitalComponents.viewMode = FixedOnOrbit
  }) // фиксация на корабле, абсолютная ориентация, в режиме карты: фиксация на орбите
  keyIgnorePause(KEY_F4, onKeyDown = if (!orbitalComponents.drawMapMode) {
    orbitalComponents.viewMode = Landing
  }) // посадка на планету, если не в режиме карты

  // функционал толком не работает
  //keyIgnorePause(KEY_F5, onKeyDown = saveGame()) // сохранить текущее состояние системы
  //keyIgnorePause(KEY_F6, onKeyDown = loadGame())                          // загрузить из файла состояние системы

  keyIgnorePause(KEY_I, onKeyDown = {
    orbitalComponents.disable_interface_drawing = !orbitalComponents.disable_interface_drawing
    if (orbitalComponents.disable_interface_drawing) InterfaceHolder.hideAllByUser()
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
      if (orbitalComponents.realTrajectory.curPoints < InterfaceHolder.realTrajectorySwitcher.numPoints) {
        InterfaceHolder.realTrajectorySwitcher.numPoints = orbitalComponents.realTrajectory.curPoints
      } else {
        InterfaceHolder.realTrajectorySwitcher.numPoints = 24 * 3600
        orbitalComponents.orbitsUpdater.scheduleOrbitsUpdate("reset real trajectory num points")
      }
    }
  })
}
