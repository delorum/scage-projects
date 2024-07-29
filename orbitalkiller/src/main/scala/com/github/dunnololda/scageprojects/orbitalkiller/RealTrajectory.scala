package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.components.{PlanetComponents, ShipComponents}
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{MutableBodyState, SystemEvolution}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

class RealTrajectory(system_evolution: SystemEvolution,
                     planetComponents: PlanetComponents,
                     shipComponents: ShipComponents,
                     max_multiplier: Option[Double]) {

  import planetComponents._
  import shipComponents._

  private var real_trajectory: ArrayBuffer[DVec] = ArrayBuffer[DVec]()
  var curPoints: Long = 0
  private var dropped = 0
  private var system_evolution_copy: SystemEvolution = _
  private var celestials: Seq[(CelestialBody, MutableBodyState)] = _
  private val angle_diff = 1
  private var prev_energy: /*Double*/ Seq[(String, Double)] = /*0.0*/ Seq.empty

  def energy: /*Double*/ Seq[(String, Double)] = {
    val all_bodies = system_evolution_copy.allBodyStates.values.toSeq
    val kinetic = all_bodies.map(b => (b.index.toString, 0.5 * b.mass * b.vel.norma2 + b.I * b.ang_vel.toRad * b.ang_vel.toRad))
    val potential = all_bodies.combinations(2).map {
      case Seq(b1, b2) => (s"${b1.index}-${b2.index}", -0.5 * G * b1.mass * b2.mass / b1.coord.dist(b2.coord))
    }.toSeq
    kinetic ++ potential
    /*val kinetic = all_bodies.map(b => 0.5*b.mass*b.vel.norma2 + b.I*b.ang_vel.toRad*b.ang_vel.toRad).sum
    val potential = all_bodies.combinations(2).map {
      case Seq(b1, b2) => - 0.5*G*b1.mass*b2.mass/b1.coord.dist(b2.coord)
    }.sum
    kinetic + potential*/
  }

  def init(): Unit = {
    real_trajectory.clear()
    curPoints = 0
    dropped = 0
    system_evolution_copy = system_evolution.copy(
      base_dt,
      exclude = immutable.Set(station.index, sat1.index, sat2.index, cargo1.index),
      collisions_enabled = false)
    celestials = system_evolution_copy.allBodyStates.filter(kv => planet_indices.contains(kv._1)).flatMap(kv => {
      system_planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
    }).values.toSeq
    prev_energy = /*0.0*/ Seq.empty
  }

  private var min_m: Double = Double.MaxValue
  private var max_m: Double = 0

  private def calcMultiplier(): Double = {
    def m: Double = {
      (for {
        ps <- system_evolution_copy.bodyState(player_ship.index)
      } yield {
        // http://arxiv.org/pdf/1105.1082.pdf
        // N-body simulations of gravitational dynamics, Walter Dehnen and Justin I. Read,
        // p. 7, 2.2.3 The choise of time-step, formula (21)
        // Квадратный корень от softening length, деленного на ускорение в точке, умноженный на коэффициент.
        // Softening length посчитал как радиус системы (380к км) поделить на квадратный корень от количества взаимодействующих тел (то есть,
        // корень из трех - Земля, Луна, корабль). Коэффициент подобран так, чтобы минимальный шаг был base_dt.
        val a = ps.acc.norma
        if (a != 0) {
          val m = 1.0 / 543200 * math.sqrt(2.2E8 / a) / base_dt
          if (m < min_m) min_m = m
          if (m > max_m) max_m = m
          m
        } else 1.0
      }).getOrElse(1.0)
    }

    max_multiplier.map(x => {
      if (x == 1) 1.0
      else {
        math.min(math.max(1, m), x)
      }
    }).getOrElse({
      math.max(1, m)
    })
  }

  protected def chooseDt: Double = {
    if (player_ship.engines.exists(_.stopMomentTacts >= system_evolution_copy.tacts)) {
      base_dt // пока работают двигатели, dt должен быть равен base_dt, иначе неверно работают формулы.
    } else {
      if (prev_energy /* == 0 */ .isEmpty) prev_energy = energy
      calcMultiplier() * base_dt
    }
  }

  def continue(): Unit = {
    if (InterfaceHolder.realTrajectorySwitcher.showRealTrajectory && InterfaceHolder.realTrajectorySwitcher.numPoints > curPoints) {
      if (real_trajectory.length >= 3) {
        val prev_line = real_trajectory(real_trajectory.length - 2) - real_trajectory(real_trajectory.length - 3)
        val cur_line = real_trajectory(real_trajectory.length - 1) - real_trajectory(real_trajectory.length - 2)
        if (cur_line.absDeg(prev_line) <= angle_diff) {
          real_trajectory.remove(real_trajectory.length - 1)
          dropped += 1
        }
      }
      // 6300 итераций - 100 секунд симуляции при базовом dt = 1/63 секунды
      var i = 1
      var seconds: Double = 0
      while (i < 6301 && curPoints + seconds < InterfaceHolder.realTrajectorySwitcher.numPoints) {
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
            } yield (player_coord - planet_coord) * scale
          case None =>
            system_evolution_copy.bodyState(player_ship.thisOrActualProxyShipIndex).map(bs => {
              player_ship.orbitData match {
                case Some(or) =>
                  system_evolution_copy.bodyState(or.planet_state.index).map(p => (bs.coord - p.coord) * scale).getOrElse(bs.coord * scale)
                case None =>
                  bs.coord * scale
              }
            })
        }) match {
          case Some(next_point) =>
            if (real_trajectory.length < 2 || i == 6300) {
              real_trajectory += next_point
            } else {
              val prev_line = real_trajectory.last - real_trajectory.init.last
              val cur_line = next_point - real_trajectory.last
              if (cur_line.absDeg(prev_line) > angle_diff) {
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
      if (curPoints > InterfaceHolder.realTrajectorySwitcher.numPoints) {
        InterfaceHolder.realTrajectorySwitcher.numPoints = curPoints
      }
      val e = energy
      val x = e.map(_._2).sum - prev_energy.map(_._2).sum
      if (max_multiplier.contains(1)) {
        println(f"real trajectory dt ${system_evolution_copy.base_dt / base_dt}%.2f*base_dt, dE=${x / prev_energy.map(_._2).sum}%.10f, curPoints/numPoints $curPoints/${InterfaceHolder.realTrajectorySwitcher.numPoints} dropped/length $dropped/${real_trajectory.length}")
      } else {
        println(f"real trajectory dt ${system_evolution_copy.base_dt / base_dt}%.2f*base_dt, dE=${x / prev_energy.map(_._2).sum}%.10f, min_m = $min_m, max_m = $max_m, curPoints/numPoints $curPoints/${InterfaceHolder.realTrajectorySwitcher.numPoints} dropped/length $dropped/${real_trajectory.length}")
      }
      prev_energy = energy
      /*println(e.zip(init_energy).map {
        case ((s1, e1), (s2, e2)) =>
          if(s1 == s2) {
            if(e2 != 0) {
              f"$s1 ${(e1 - e2) / x * 100}%.5f"
            } else {
              s"$s1 N/A"
            }
          } else s"$s1 != $s2"
      }.mkString(f"${x/init_energy.map(_._2).sum}%.5f : ", " : ", ""))*/
    }
  }

  def realTrajectory: Seq[DVec] = real_trajectory
}
