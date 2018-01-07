package com.github.dunnololda.scageprojects.orbitalkiller.render

import com.github.dunnololda.scageprojects.orbitalkiller.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.{EllipseOrbit, HyperbolaOrbit, KeplerOrbit}

case class OrbitRenderData(update_count: Long,
                           body_state: MutableBodyState,
                           bs_radius: Double,
                           planet_state: MutableBodyState,
                           planet: CelestialBody,
                           orbit: KeplerOrbit,
                           ccw: Boolean,
                           render: () => Unit) {
  lazy val ellipseOrbit: Option[EllipseOrbit] = orbit match {
    case x: EllipseOrbit => Some(x)
    case _ => None
  }
  lazy val hyperbolaOrbit: Option[HyperbolaOrbit] = orbit match {
    case x: HyperbolaOrbit => Some(x)
    case _ => None
  }

  val init_bs_coord = body_state.coord
  val init_bs_vel = body_state.vel
  val init_planet_coord = planet_state.coord
  val init_planet_vel = planet_state.vel
  val init_planet_ang = planet_state.ang

  lazy val planet_radius_plus_body_radius_sq = (planet.radius + bs_radius) * (planet.radius + bs_radius)
  lazy val is_landed: Boolean = {
    init_bs_coord.dist2(init_planet_coord) <= planet_radius_plus_body_radius_sq &&
      ((init_bs_vel - init_planet_vel) * (init_bs_coord - init_planet_coord).n).abs < 0.5 &&
      (((init_bs_vel - init_planet_vel) * (init_bs_coord - init_planet_coord).p) / init_bs_coord.dist(init_planet_coord) * planet.radius - planet.groundSpeedMsec).abs < 0.5
  }

  lazy val is_landed_on_earth: Boolean = is_landed && planet.index == earthIndex
  lazy val is_landed_on_moon: Boolean = is_landed && planet.index == moonIndex

  lazy val orbitStrDefinition: String = {
    if (is_landed) "landed"
    else {
      orbit.strDefinition(planet.name,
        planet.radius,
        init_planet_vel,
        init_planet_ang,
        planet.groundSpeedMsec,
        planet.g,
        init_bs_coord,
        init_bs_vel,
        bs_radius)
    }
  }
}
