package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ObjectIndices._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.{CelestialBody, Planet, PlanetWithAir, Star}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics.OrbitUtils.speedToHaveOrbitWithParams

trait CelestialsSupport extends CelestialsAware with SystemEvolutionAware {

  val sun: Star = new Star(
    sunIndex,
    "Солнце",
    mass = 1.9891e30,
    coord = DVec(0, 1.496e11),
    radius = 6.9551e8
  )

  private val earth_start_position = DVec.dzero

  private val earth_init_velocity =
    speedToHaveOrbitWithParams(earth_start_position, 0, sun.coord, sun.linearVelocity, sun.mass, ccw = true)

  val earth: PlanetWithAir = new PlanetWithAir(
    index = earthIndex,
    name = "Земля",
    mass = 5.9746e24,
    init_coord = earth_start_position,
    init_velocity = earth_init_velocity,
    // init_ang_vel = 0.0,
    init_ang_vel = 360.0 / (24L * 60 * 60),
    radius = 6400000 /*6314759.95726045*/,
    orbiting_body = sun,
    air_free_altitude = 101000,
    T0 = 288,
    L = 0.00288 /*0.0065*/,
    P0 = 101325,
    M = 0.02896,
    R = 8.314
  )

  private val moon_start_position = earth.coord + DVec(0, 1).rotateDeg(280) * 380000000L

  private val moon_init_velocity =
    speedToHaveOrbitWithParams(moon_start_position, 0, earth.coord, earth.linearVelocity, earth.mass, ccw = true)

  val moon: Planet = new Planet(
    moonIndex,
    "Луна",
    mass = 7.3477e22,
    init_coord = moon_start_position,
    init_velocity = moon_init_velocity,
    // период орбиты луны в данной симуляции: 26 д. 8 ч. 59 мин. 44 сек, равен периоду обращения вокруг собственной оси
    init_ang_vel = 360.0 / (26L * 24 * 60 * 60 + 8L * 60 * 60 + 59L * 60 + 44),
    radius = 1737000,
    earth,
    2000
  )

  systemEvolution.addBody(
    moon.currentState,
    (_, helper) => {
      helper.gravityForceFromTo(sun.index, moon.index) +
        helper.gravityForceFromTo(earth.index, moon.index)
    },
    (_, _) => {
      0.0
    }
  )

  systemEvolution.addBody(
    earth.currentState,
    (_, helper) => {
      helper.gravityForceFromTo(sun.index, earth.index) +
        helper.gravityForceFromTo(moon.index, earth.index)
    },
    (_, _) => {
      0.0
    }
  )

  systemEvolution.addBody(
    sun.currentState,
    (_, helper) => {
      helper.gravityForceFromTo(earth.index, sun.index) +
        helper.gravityForceFromTo(moon.index, sun.index)
    },
    (_, _) => {
      0.0
    }
  )

  systemEvolution.addCollisionExclusion(earth.index, moon.index)
  systemEvolution.addCollisionExclusion(earth.index, sun.index)
  systemEvolution.addCollisionExclusion(moon.index, sun.index)

  private val allCelestials: Seq[CelestialBody] = Seq(sun, earth, moon)

  val celestialsHelper: CelestialsHelper = new CelestialsHelper(allCelestials, systemEvolution)
}
