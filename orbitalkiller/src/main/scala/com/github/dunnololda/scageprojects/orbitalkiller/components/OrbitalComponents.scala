package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.RealTrajectory
import com.github.dunnololda.scageprojects.orbitalkiller.physics.SystemEvolution
import BasicComponents._

/**
  * Created by andrey on 1/6/18.
  */
class OrbitalComponents extends SystemEvolutionAware with PlanetComponents with ShipComponents {
  val G: Double = 6.6742867E-11

  val k: Double = 1 // доля секунды симуляции, которая обрабатывается за одну реальную секунду, если не применяется ускорение

  val linear_velocity_error: Double = 0.1
  val angular_velocity_error: Double = 0.0102
  // значение подобрано эмпирически при тестах с малым количеством топлива
  val angle_error = 0.1

  val scale: Double = 1e-6

  val system_evolution: SystemEvolution = new SystemEvolution(base_dt)

  val realTrajectory = new RealTrajectory(this, None)

  def nameByIndex(index: Int): Option[String] = {
    system_planets.get(index) match {
      case s@Some(x) => s.map(_.name)
      case None => shipsHolder.shipByIndex(index).map(_.name)
    }
  }
}
