package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitDataUpdater, RealTrajectory}
import com.github.dunnololda.scageprojects.orbitalkiller.physics.SystemEvolution
import BasicComponents._

/**
  * Created by andrey on 1/6/18.
  */
class OrbitalComponents {
  val system_evolution: SystemEvolution = new SystemEvolution(base_dt)

  val planetComponents = new PlanetComponents(system_evolution)

  val shipComponents = new ShipComponents(system_evolution, planetComponents)

  val realTrajectory = new RealTrajectory(system_evolution, planetComponents, shipComponents, None)

  val systemEvolutionComponents = new SystemEvolutionComponents(system_evolution, shipComponents)

  val orbitDataUpdater = new OrbitDataUpdater(planetComponents, shipComponents, realTrajectory)

  def nameByIndex(index: Int): Option[String] = {
    planetComponents.system_planets.get(index) match {
      case s@Some(x) => s.map(_.name)
      case None => shipComponents.shipByIndex(index).map(_.name)
    }
  }
}
