package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.RealTrajectory
import com.github.dunnololda.scageprojects.orbitalkiller.physics.SystemEvolution
import BasicComponents._
import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.render.OrbitRenderDataUpdater
import com.github.dunnololda.scageprojects.orbitalkiller.util.TactsCounter

/**
  * Created by andrey on 1/6/18.
  */
class OrbitalComponents extends DrawMapMode with ViewModeComponents {
  val system_evolution: SystemEvolution = new SystemEvolution(base_dt)

  val planetComponents = new PlanetComponents(system_evolution)

  val shipComponents = new ShipComponents(system_evolution, planetComponents)

  val realTrajectory = new RealTrajectory(system_evolution, planetComponents, shipComponents, None)

  val systemTimer = new SystemTimer(system_evolution)

  val futureStateCalculator = new FutureStateCalculator(system_evolution, shipComponents)

  val orbitRenderDataUpdater = new OrbitRenderDataUpdater(planetComponents, shipComponents, realTrajectory)

  val orbitsUpdater = new OrbitsUpdater(system_evolution, systemTimer, futureStateCalculator, realTrajectory, planetComponents, shipComponents, orbitRenderDataUpdater)

  val stop_after_number_of_tacts: TactsCounter = new TactsCounter()
  var _stop_in_orbit_true_anomaly: Double = 0
  var left_up_corner: Option[DVec] = None
  var right_down_corner: Option[DVec] = None
  var set_stop_moment = false

  val timeMultiplier = new TimeMultiplier(shipComponents)

  def nameByIndex(index: Int): Option[String] = {
    planetComponents.system_planets.get(index) match {
      case s@Some(x) => s.map(_.name)
      case None => shipComponents.shipByIndex(index).map(_.name)
    }
  }
}
