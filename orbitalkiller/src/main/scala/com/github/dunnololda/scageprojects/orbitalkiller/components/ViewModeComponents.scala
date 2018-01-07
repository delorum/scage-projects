package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.KeplerOrbit._
import BasicComponents._

/**
  * Created by andrey on 1/7/18.
  */
trait ViewModeComponents {
  this: OrbitalComponents =>
  private var view_mode: ViewMode = FreeViewMode

  def viewMode = view_mode

  def viewMode_=(new_view_mode: ViewMode) {
    if (new_view_mode != view_mode) {
      val player_ship = shipComponents.player_ship
      val earth = planetComponents.earth
      val moon = planetComponents.moon
      val currentPlanetStates = planetComponents.currentPlanetStates
      new_view_mode match {
        case FreeViewMode => // свободный
          _center = center
          center = _center
          rotationAngle = 0
          base = DVec.zero
          view_mode = FreeViewMode
        case FixedOnShip => // фиксация на корабле
          center = player_ship.coord + _ship_offset
          base = if (player_ship.coord.norma < 100000) DVec.zero else player_ship.coord
          rotationCenter = player_ship.coord
          rotationAngleDeg = -player_ship.rotation
          view_mode = FixedOnShip
        case Landing => // посадка на планету
          center = player_ship.coord + _ship_offset
          rotationCenter = player_ship.coord
          rotationAngleDeg = {
            val nearest_body_coord = if (player_ship.coord.dist2(earth.coord) < player_ship.coord.dist2(moon.coord)) earth.coord else moon.coord
            val vec = player_ship.coord - nearest_body_coord
            if (vec.x >= 0) vec.deg(DVec(0, 1))
            else vec.deg(DVec(0, 1)) * (-1)
          }
          view_mode = Landing
        case FixedOnShipAbsolute => // фиксация на корабле, абсолютная ориентация
          center = player_ship.coord + _ship_offset
          base = if (player_ship.coord.norma < 100000) DVec.zero else player_ship.coord
          rotationAngle = 0
          view_mode = FixedOnShipAbsolute
        case FixedOnOrbit => // в режиме карты зафиксировать центр орбиты в центре экрана
          if (drawMapMode) {
            _center = _center - orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates)
              .map(_._2.center * scale)
              .getOrElse(player_ship.coord)
            center = orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates)
              .map(_._2.center * scale)
              .getOrElse(player_ship.coord) + _center
            rotationAngle = 0
            view_mode = FixedOnOrbit
          }
        case _ =>
      }
    }
  }
}
