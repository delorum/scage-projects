package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.RealTrajectory
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.render.OrbitRenderDataUpdater
import com.github.dunnololda.scageprojects.orbitalkiller.vessels.Maneuvering
import BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.SystemEvolution

/**
  * Created by andrey on 1/7/18.
  */
class OrbitsUpdater(system_evolution: SystemEvolution,
                    systemEvolutionComponents: SystemEvolutionComponents,
                    realTrajectory: RealTrajectory,
                    planetComponents: PlanetComponents,
                    shipComponents: ShipComponents,
                    orbitRenderDataUpdater: OrbitRenderDataUpdater) {

  import shipComponents._
  import planetComponents._

  var _update_orbits = false
  var update_count: Long = 0l

  def updateOrbits() {
    //println("updateOrbits")
    if (player_ship.flightMode == Maneuvering || !onPause || !player_ship.engines.exists(_.active)) {
      // если в режиме маневрирования, или не в режиме маневрирования, но не на паузе, или на паузе, но двигатели не работают - рисуем текущее состояние
      moon.orbitRender = orbitRenderDataUpdater.updateOrbitData(
        update_count, moon.index, moon.radius, player_ship.colorIfPlayerAliveOrRed(GREEN), system_evolution.allBodyStates, Set(earth.index, sun.index), None)
      earth.orbitRender = orbitRenderDataUpdater.updateOrbitData(
        update_count, earth.index, earth.radius, player_ship.colorIfPlayerAliveOrRed(ORANGE), system_evolution.allBodyStates, Set(sun.index), None)
      player_ship.updateOrbitData(
        update_count, player_ship.colorIfPlayerAliveOrRed(YELLOW), systemEvolutionComponents.timeMsec, system_evolution.allBodyStates, InterfaceHolder.orbitSwitcher.calculateOrbitAround)
      InterfaceHolder.orbitInfo.markUpdateNeeded()
      InterfaceHolder.shipInterfaces.foreach(si => {
        if (!si.isMinimized && !si.monitoring_ship.isCrashed) {
          si.monitoring_ship.updateOrbitData(
            update_count, player_ship.colorIfPlayerAliveOrRed(MAGENTA), systemEvolutionComponents.timeMsec, system_evolution.allBodyStates)
          si.markUpdateNeeded()
        }
      })
    } else {
      // в эту секцию мы попадаем, если мы не в режиме маневрирования, на паузе, и двигатели работают
      val stop_moment_tacts = player_ship.engines.map(_.stopMomentTacts).max
      val system_state_when_engines_off = systemEvolutionComponents.getFutureState(stop_moment_tacts)
      moon.orbitRender = orbitRenderDataUpdater.updateOrbitData(update_count, moon.index, moon.radius, player_ship.colorIfPlayerAliveOrRed(GREEN), system_state_when_engines_off, Set(earth.index, sun.index), None)
      earth.orbitRender = orbitRenderDataUpdater.updateOrbitData(update_count, earth.index, earth.radius, player_ship.colorIfPlayerAliveOrRed(ORANGE), system_state_when_engines_off, Set(sun.index), None)
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

  def needToUpdateOrbits(reason: String) {
    println(s"needToUpdateOrbits: $reason")
    if (onPause) {
      systemEvolutionComponents.system_cache.clear()
      _update_orbits = true
      realTrajectory.init()
      //RealTrajectory2.init()
      //RealTrajectory3.init()
    }
  }
}
