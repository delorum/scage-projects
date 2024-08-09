package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ObjectIndices.planetIndices
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.orbits.KeplerOrbit
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.orbits.KeplerOrbit.calculateOrbit
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.SystemEvolution
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics.GravityUtils.insideSphereOfInfluenceOfCelestialBody

import scala.collection.{immutable, Map}

class CelestialsHelper(allCelestials: Seq[CelestialBody], systemEvolution: SystemEvolution) {
  val planets: immutable.Map[Int, CelestialBody] = allCelestials.map(c => c.index -> c).toMap

  def planetByIndex(index: Int): Option[CelestialBody] = planets.get(index)

  def planetStates(system_state: Map[Int, MutableBodyState]): Seq[(CelestialBody, MutableBodyState)] = {
    system_state
      .flatMap(kv => {
        planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
      })
      .values
      .toSeq
      .sortBy(_._2.mass)
  }

  val currentPlanetStates: Seq[(CelestialBody, MutableBodyState)] = planetStates(
    systemEvolution.bodyStates(planetIndices)
  )

  def orbitAroundCelestialInPointWithVelocity(
      coord: DVec,
      velocity: DVec,
      mass: Double): Option[((CelestialBody, MutableBodyState), KeplerOrbit)] = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((_, planet_state)) =>
        planetByIndex(planet_state.index).flatMap(planet => {
          Some(
            (
              (planet, planet_state),
              calculateOrbit(
                planet_state.mass,
                planet_state.coord,
                mass,
                coord - planet_state.coord,
                velocity - planet_state.vel
              )
            )
          )
        })
      case None => None
    }
  }
}
