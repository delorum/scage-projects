package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents.base_dt
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{MutableBodyState, SystemEvolution}
import com.github.dunnololda.scageprojects.orbitalkiller.util.LogUtils
import com.github.dunnololda.scageprojects.orbitalkiller.vessels.Maneuvering
import BasicComponents.angular_velocity_error
import scala.collection.mutable

/**
  * TODO
  *
  * @author aborunov
  */
class FutureStateCalculator(system_evolution: SystemEvolution,
                            shipComponents: ShipComponents) {
  import shipComponents._

  private val future_cache: mutable.HashMap[Long, mutable.Map[Int, MutableBodyState]] = {
    mutable.HashMap[Long, mutable.Map[Int, MutableBodyState]]()
  }

  def clearCache(): Unit = {
    future_cache.clear()
  }

  def getFutureState(tacts: Long): mutable.Map[Int, MutableBodyState] = {
    if (shipComponents.player_ship.flightMode != Maneuvering) {
      future_cache.getOrElseUpdate(tacts, {
        LogUtils.log("adding to future_cache")
        val system_evolution_copy = system_evolution.copy(base_dt)
        val steps = tacts - system_evolution_copy.tacts
        (1l to steps).foreach(x => {
          system_evolution_copy.allBodyStates.map(bs => (bs._2, shipByIndex(bs._2.index))).foreach(bs => {
            if (bs._1.ang_vel != 0 && math.abs(bs._1.ang_vel) < angular_velocity_error) {
              bs._1.ang_vel = 0
            }
            bs._2.foreach(s => {
              bs._1.mass = s.thisOrActualProxyShipCurrentMass(system_evolution_copy.tacts)
            })
          })
          system_evolution_copy.step()
        })
        system_evolution_copy.allBodyStates
      })
    } else collection.mutable.Map()
  }
}
