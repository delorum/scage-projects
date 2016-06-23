package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import OrbitalKiller._

object RealTrajectory extends RealTrajectoryC
object RealTrajectory2 extends RealTrajectoryC {
  override protected def chooseDt:Double = base_dt
}

class RealTrajectoryC {
  private var real_trajectory:ArrayBuffer[DVec] = ArrayBuffer[DVec]()
  var curPoints:Long = 0
  private var dropped = 0
  private var system_evolution_copy:SystemEvolution = _
  private var celestials:Seq[(CelestialBody, MutableBodyState)] = _

  private val angle_diff = 1

  def init(): Unit = {
    real_trajectory.clear()
    curPoints = 0
    dropped = 0
    system_evolution_copy = system_evolution.copy(
      base_dt,
      exclude = immutable.Set(station.index, sat1.index, sat2.index, cargo1.index),
      collisions_enabled = false)
    celestials = system_evolution_copy.allBodyStates.filter(kv => planet_indices.contains(kv._1)).flatMap(kv => {
      planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
    }).values.toSeq
  }

  protected def chooseDt:Double = {
    // выберем dt по критерию расстояния корабля до ближайшей поверхности: в 100 км от поверхности объект, движущийся со скоростью 30 км/сек
    // за dt должен пролететь не более 500 метров (это справедливо для base_dt). Чем дальше, тем dt может быть больше.
    // но пока работают двигатели, dt должен быть равен base_dt, иначе неверно работают формулы.
    if(player_ship.engines.exists(_.stopMomentTacts > system_evolution_copy.tacts)) {
      base_dt
    } else {
      system_evolution_copy.bodyState(player_ship.thisOrActualProxyShipIndex).map(bs => {
        val min_dist_to_any_surface = celestials.map(x => bs.coord.dist(x._2.coord) - x._1.radius).min
        //0.005 * math.log(min_dist_to_any_surface / bs.vel.norma) / math.log(10 / 3) * 10 / 3
        0.005 * min_dist_to_any_surface / bs.vel.norma
      }).getOrElse(base_dt)
    }
  }

  def continue(): Unit = {
    if(InterfaceHolder.realTrajectorySwitcher.showRealTrajectory && InterfaceHolder.realTrajectorySwitcher.numPoints > curPoints) {
      val new_base_dt = chooseDt
      system_evolution_copy.base_dt = new_base_dt
      // выбираем так, чтобы в цикле было ровно 100 итераций.
      val seconds_in_this_iteration = math.min((6300*system_evolution_copy.base_dt).toInt, InterfaceHolder.realTrajectorySwitcher.numPoints - curPoints)
      val last_step = math.max(1, (seconds_in_this_iteration*(1/system_evolution_copy.base_dt)).toInt)
      (1 to last_step).foreach(step => {
        system_evolution_copy.bodyState(player_ship.index).foreach(bs => {
          if (bs.ang_vel != 0 && math.abs(bs.ang_vel) < angular_velocity_error) {
            bs.ang_vel = 0
          }
          bs.mass = player_ship.thisOrActualProxyShipCurrentMass(system_evolution_copy.tacts)
        })
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
              if(cur_line.absDeg(prev_line) > angle_diff) {
                real_trajectory += next_point
              } else {
                dropped += 1
              }
            }
          case None =>
        }
      })
      curPoints += seconds_in_this_iteration
      println(f"real trajectory dt ${new_base_dt/base_dt}%.2f*base_dt curPoints/numPoints $curPoints/${InterfaceHolder.realTrajectorySwitcher.numPoints} dropped/length $dropped/${real_trajectory.length}")
    }
  }

  def realTrajectory:Seq[DVec] = real_trajectory
}
