package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.{MutableBodyState, SystemEvolution}
import com.github.dunnololda.scageprojects.orbitalkiller.celestials.{CelestialBody, Planet, PlanetWithAir, Star}
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.PhysicsUtils._

import scala.collection.{Map, Seq, immutable}

/**
  * Created by andrey on 1/7/18.
  */
class PlanetComponents(system_evolution: SystemEvolution) {
  val sun: Star = new Star(
    sunIndex, "Солнце",
    mass = 1.9891E30,
    coord = DVec(0, 1.496E11),
    radius = 6.9551E8
  )

  val earth_start_position = DVec.dzero
  val earth_init_velocity = speedToHaveOrbitWithParams(earth_start_position, 0, sun.coord, sun.linearVelocity, sun.mass, G, ccw = true)
  val earth: PlanetWithAir = new PlanetWithAir(
    earthIndex, name = "Земля",
    mass = 5.9746E24,
    init_coord = earth_start_position,
    init_velocity = earth_init_velocity,
    //init_ang_vel = 0.0,
    init_ang_vel = 360.0 / (24l * 60 * 60),
    radius = 6400000 /*6314759.95726045*/ ,
    orbiting_body = sun, air_free_altitude = 101000,
    T0 = 288,
    L = 0.00288 /*0.0065*/ ,
    P0 = 101325,
    M = 0.02896,
    R = 8.314)

  val moon_start_position = earth.coord + DVec(0, 1).rotateDeg(280) * 380000000l
  val moon_init_velocity = speedToHaveOrbitWithParams(moon_start_position, 0, earth.coord, earth.linearVelocity, earth.mass, G, ccw = true)
  val moon: Planet = new Planet(
    moonIndex, "Луна",
    mass = 7.3477E22,
    init_coord = moon_start_position,
    init_velocity = moon_init_velocity,
    init_ang_vel = 360.0 / (26l * 24 * 60 * 60 + 8l * 60 * 60 + 59l * 60 + 44), // период орбиты луны в данной симуляции: 26 д. 8 ч. 59 мин. 44 сек, равен периоду обращения вокруг собственной оси
    radius = 1737000,
    earth, 2000)

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
