package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{MutableBodyState, SystemEvolution}
import com.github.dunnololda.scageprojects.orbitalkiller.vessels.Maneuvering

import scala.collection.mutable

/**
  * Created by andrey on 1/7/18.
  */
class SystemEvolutionComponents(system_evolution: SystemEvolution, ships: ShipComponents) extends TimeAware {

  import ships._

  def tacts: Long = system_evolution.tacts

  def timeMsec: Long = (tacts * base_dt * 1000).toLong

  def currentBodyState(index: Int): Option[MutableBodyState] = system_evolution.bodyState(index)

  val system_cache = mutable.HashMap[Long, mutable.Map[Int, MutableBodyState]]()

  def getFutureState(tacts: Long): mutable.Map[Int, MutableBodyState] = {
    if (player_ship.flightMode != Maneuvering) {
      system_cache.getOrElseUpdate(tacts, {
        println("adding to system_cache")
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
