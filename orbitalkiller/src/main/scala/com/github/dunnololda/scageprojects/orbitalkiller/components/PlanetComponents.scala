package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{MutableBodyState, SystemEvolution}
import com.github.dunnololda.scageprojects.orbitalkiller.celestials._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.PhysicsUtils._

import scala.collection.{Map, Seq, immutable}

/**
  * Created by andrey on 1/7/18.
  */
class PlanetComponents(system_evolution: SystemEvolution) {
  val sun: Sun = new Sun

  private val earth_start_position = DVec.dzero
  private val earth_init_velocity = speedToHaveOrbitWithParams(earth_start_position, 0, sun.coord, sun.linearVelocity, sun.mass, G, ccw = true)
  val earth: Earth = new Earth(
    earth_start_position,
    earth_init_velocity,
    sun)

  private val moon_start_position = earth.coord + DVec(0, 1).rotateDeg(280) * 380000000l
  private val moon_init_velocity = speedToHaveOrbitWithParams(moon_start_position, 0, earth.coord, earth.linearVelocity, earth.mass, G, ccw = true)
  val moon: Moon = new Moon(
    moon_start_position,
    moon_init_velocity,
    earth)

  system_evolution.addBody(
    moon.currentState,
    (tacts, helper) => {
      helper.gravityForceFromTo(sun.index, moon.index) +
        helper.gravityForceFromTo(earth.index, moon.index)
    },
    (tacts, helper) => {
      0.0
    }
  )

  system_evolution.addBody(
    earth.currentState,
    (tacts, helper) => {
      helper.gravityForceFromTo(sun.index, earth.index) +
        helper.gravityForceFromTo(moon.index, earth.index)
    },
    (tacts, helper) => {
      0.0
    }
  )

  system_evolution.addBody(
    sun.currentState,
    (tacts, helper) => {
      helper.gravityForceFromTo(earth.index, sun.index) +
        helper.gravityForceFromTo(moon.index, sun.index)
    },
    (tacts, helper) => {
      0.0
    }
  )

  system_evolution.addCollisionExclusion(earth.index, moon.index)
  system_evolution.addCollisionExclusion(earth.index, sun.index)
  system_evolution.addCollisionExclusion(moon.index, sun.index)

  val earth_sun_eq_gravity_radius = equalGravityRadius(earth.currentState, sun.currentState)
  val moon_earth_eq_gravity_radius = equalGravityRadius(moon.currentState, earth.currentState)

  val system_planets = immutable.Map(sun.index -> sun,
    earth.index -> earth,
    moon.index -> moon)
  val planet_indices: immutable.Set[Int] = system_planets.keySet

  def planetStates(body_states: Map[Int, MutableBodyState]): Seq[(CelestialBody, MutableBodyState)] = {
    body_states.flatMap(kv => {
      system_planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
    }).values.toSeq.sortBy(_._2.mass)
  }

  val currentPlanetStates: Seq[(CelestialBody, MutableBodyState)] = planetStates(system_evolution.bodyStates(planet_indices))

  def planetByIndex(index: Int): Option[CelestialBody] = system_planets.get(index)
}
