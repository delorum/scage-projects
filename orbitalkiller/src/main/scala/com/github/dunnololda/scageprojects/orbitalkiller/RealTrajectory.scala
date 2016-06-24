package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import OrbitalKiller._

object RealTrajectory extends RealTrajectoryC(None)
object RealTrajectory2 extends RealTrajectoryC(Some(1))
object RealTrajectory3 extends RealTrajectoryC(Some(100))

class RealTrajectoryC(max_multiplier:Option[Double]) {
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

  private var min_m:Double = Double.MaxValue
  private var max_m:Double = 0

  protected def chooseDt:Double = {
    if(player_ship.engines.exists(_.stopMomentTacts >= system_evolution_copy.tacts)) {
      base_dt // пока работают двигатели, dt должен быть равен base_dt, иначе неверно работают формулы.
    } else {
      if(max_multiplier.exists(_ == 1)) base_dt
      else {
        (for {
          ss <- system_evolution_copy.bodyState(sun.index)
          es <- system_evolution_copy.bodyState(earth.index)
          ms <- system_evolution_copy.bodyState(moon.index)
          ps <- system_evolution_copy.bodyState(player_ship.index)
        } yield {
          // http://arxiv.org/pdf/1105.1082.pdf
          // N-body simulations of gravitational dynamics, Walter Dehnen and Justin I. Read,
          // p. 7, 2.2.3 The choise of time-step, formula (20)
          // Считаем гравитационный потенциал в точке, от модуля извлекаем квадртаный корень и делим на модуль ускорения корабля.
          // И умножаем на коэффициент. Эмпирически подобрал коэффициент 1/25565. При этом минимальный dt будет base_dt (1/63 секунды),
          // максимальный в 6000 больше.
          val x = math.sqrt(G * (/*sun.mass/ss.coord.dist(ps.coord) +
                             */ earth.mass / es.coord.dist(ps.coord) +
            moon.mass / ms.coord.dist(ps.coord))) / ps.acc.norma
          //val m = x/25565/base_dt
          val m = x / 5600 / base_dt
          if (m < min_m) min_m = m
          if (m > max_m) max_m = m
          println(f"x = $m*base_dt, min_m = $min_m, max_m = $max_m")
          if(max_multiplier.isEmpty) m * base_dt
          else math.min(m, max_multiplier.get)*base_dt
        }).getOrElse(base_dt)
      }
      /*val multiplier = system_evolution_copy.bodyState(player_ship.thisOrActualProxyShipIndex).map(bs => {
        val (min_dist_to_any_surface, planet_mass) = celestials.map(x => (bs.coord.dist(x._2.coord) - x._1.radius, x._2.mass)).minBy(_._1)
        if(min_dist_to_any_surface >= 10000000) {
          math.max(1.0, math.min(30000.0/100000 * min_dist_to_any_surface / bs.vel.norma, max_multiplier.getOrElse(Double.MaxValue)))
        } else {
          math.max(1.0, math.min(30000.0/100000 * min_dist_to_any_surface / bs.vel.norma * moon.mass / planet_mass, max_multiplier.getOrElse(Double.MaxValue)))
        }
      }).getOrElse(1.0)
      multiplier*base_dt*/
    }
  }

  def continue(): Unit = {
    if(InterfaceHolder.realTrajectorySwitcher.showRealTrajectory && InterfaceHolder.realTrajectorySwitcher.numPoints > curPoints) {
      if(real_trajectory.length >= 3) {
        val prev_line = real_trajectory(real_trajectory.length-2) - real_trajectory(real_trajectory.length-3)
        val cur_line = real_trajectory(real_trajectory.length-1) - real_trajectory(real_trajectory.length-2)
        if (cur_line.absDeg(prev_line) <= angle_diff) {
          real_trajectory.remove(real_trajectory.length-1)
          dropped += 1
        }
      }
      // 6300 итераций - 100 секунд симуляции при базовом dt = 1/63 секунды
      var i = 1
      var seconds:Double = 0
      while(i < 6301 && curPoints+seconds < InterfaceHolder.realTrajectorySwitcher.numPoints) {
        system_evolution_copy.base_dt = chooseDt
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
            if(real_trajectory.length < 2 || i == 6300) {
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
        seconds += system_evolution_copy.base_dt
        i += 1
      }
      curPoints += seconds.toLong
      if(curPoints > InterfaceHolder.realTrajectorySwitcher.numPoints) {
        InterfaceHolder.realTrajectorySwitcher.numPoints = curPoints
      }
      println(f"real trajectory dt ${system_evolution_copy.base_dt/base_dt}%.2f*base_dt curPoints/numPoints $curPoints/${InterfaceHolder.realTrajectorySwitcher.numPoints} dropped/length $dropped/${real_trajectory.length}")
    }
  }

  def realTrajectory:Seq[DVec] = real_trajectory
}
