package com.github.dunnololda.scageprojects.orbitalkiller.celestials

import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents.earthIndex

/**
  * TODO
  *
  * @author aborunov
  */
class Earth(earth_start_position: DVec,
            earth_init_velocity: DVec,
            sun: Sun) extends PlanetWithAir(
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
