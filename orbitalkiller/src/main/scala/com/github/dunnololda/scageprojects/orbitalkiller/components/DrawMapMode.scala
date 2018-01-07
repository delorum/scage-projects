package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.BoxShape
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.KeplerOrbit._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.{EllipseOrbit, HyperbolaOrbit}
import com.github.dunnololda.scageprojects.orbitalkiller.{FixedOnOrbit, FixedOnShip, FreeViewMode}

/**
  * Created by andrey on 1/7/18.
  */
trait DrawMapMode {
  this: OrbitalComponents =>
  var _center = shipComponents.player_ship.coord
  var _ship_offset = DVec.zero
  var disable_interface_drawing = false

  def shipOffset = _ship_offset

  private var _draw_map_mode = false

  def drawMapMode = _draw_map_mode

  def drawMapMode_=(new_mode: Boolean) {
    if (new_mode) {
      _draw_map_mode = true
      val player_ship = shipComponents.player_ship
      val currentPlanetStates = planetComponents.currentPlanetStates
      val earth = planetComponents.earth
      orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates) match {
        case Some((planet, kepler_orbit)) =>
          kepler_orbit match {
            case ellipse: EllipseOrbit =>
              val b = BoxShape(2 * ellipse.a, 2 * ellipse.b)
              val aabb = b.aabb(ellipse.center, Vec(-1, 0).signedDeg(ellipse.f2 - ellipse.f))
              viewMode = FreeViewMode
              globalScale = 750 / (aabb.height * scale)
              _center = ellipse.center * scale
              viewMode = FixedOnOrbit
            case hyperbola: HyperbolaOrbit =>
              val b = BoxShape(2 * hyperbola.a, 2 * hyperbola.b)
              val aabb = b.aabb(hyperbola.half_center, Vec(1, 0).signedDeg(hyperbola.f_minus_center_n))
              viewMode = FreeViewMode
              globalScale = 750 / (aabb.height * scale)
              _center = hyperbola.half_center * scale
              viewMode = FixedOnOrbit
          }
        case None =>
          viewMode = FreeViewMode
          globalScale = 1
          _center = earth.coord * scale
          viewMode = FixedOnOrbit
      }
    } else {
      _draw_map_mode = false
      globalScale = 10
      viewMode = FixedOnShip
    }
  }
}
