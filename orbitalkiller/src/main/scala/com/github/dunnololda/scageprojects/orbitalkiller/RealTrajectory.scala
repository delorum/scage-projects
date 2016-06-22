package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import OrbitalKiller._

object RealTrajectory {
  private var real_trajectory:ArrayBuffer[DVec] = ArrayBuffer[DVec]()
  var curPoints:Int = 0
  private var dropped = 0
  private var system_evolution_copy:SystemEvolution = _
  private val seconds_per_iteration = 100

  def init(): Unit = {
    real_trajectory.clear()
    curPoints = 0
    dropped = 0
    system_evolution_copy = system_evolution.copy(
      base_dt,
      exclude = immutable.Set(station.index, sat1.index, sat2.index, cargo1.index),
      collisions_enabled = false)
  }

  def continue(): Unit = {
    if(InterfaceHolder.realTrajectorySwitcher.showRealTrajectory && InterfaceHolder.realTrajectorySwitcher.numPoints > curPoints) {
      val seconds_in_this_iteration = math.min(seconds_per_iteration, InterfaceHolder.realTrajectorySwitcher.numPoints - curPoints)
      val last_step = (seconds_in_this_iteration*(1/OrbitalKiller.base_dt)).toInt
      if(real_trajectory.length >= 3) {
        val prev_line = real_trajectory(real_trajectory.length-2) - real_trajectory(real_trajectory.length-3)
        val cur_line = real_trajectory(real_trajectory.length-1) - real_trajectory(real_trajectory.length-2)
        if (cur_line.absDeg(prev_line) <= 10) {
          real_trajectory.remove(real_trajectory.length-1)
          dropped += 1
        }
      }
      (1 to last_step).foreach(step => {
        system_evolution_copy.step()
        (InterfaceHolder.orbitSwitcher.calculateOrbitAround match {
          case Some(idx) =>
            for {
              player_coord <- system_evolution_copy.bodyState(player_ship.thisOrActualProxyShipIndex).map(_.coord)
              planet_coord <- system_evolution_copy.bodyState(idx).map(_.coord)
            } yield (player_coord - planet_coord)*scale
          case None =>
            system_evolution_copy.bodyState(player_ship.thisOrActualProxyShipIndex).map(bs => {
              player_ship.orbitData match {
                case Some(or) =>
                  (bs.coord - or.planet_state.coord)*scale
                case None =>
                  bs.coord*scale
              }
            })
        }) match {
          case Some(next_point) =>
            if(real_trajectory.length < 2 || step == last_step) {
              real_trajectory += next_point
            } else {
              val prev_line = real_trajectory.last - real_trajectory.init.last
              val cur_line = next_point - real_trajectory.last
              if(cur_line.absDeg(prev_line) > 10) {
                real_trajectory += next_point
              } else {
                dropped += 1
              }
            }
          case None =>
        }
      })
      curPoints += seconds_in_this_iteration
      println(s"real trajectory curPoints/numPoints dropped/length: $curPoints/${InterfaceHolder.realTrajectorySwitcher.numPoints} $dropped/${real_trajectory.length}")
    }
  }

  def realTrajectory:Seq[DVec] = real_trajectory
}
