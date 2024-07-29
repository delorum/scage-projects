package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit.KeplerOrbit._
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._
import com.github.dunnololda.scageprojects.orbitalkiller.components.{FixedOnOrbit, OrbitalComponentsAware}
import com.github.dunnololda.scageprojects.orbitalkiller.vessels.Ship4

/**
  * Created by andrey on 1/7/18.
  */
trait OrbitMouseControls extends OrbitalComponentsAware {
  this: ScageScreenAppDMT =>

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if (globalScale > 0.01) {
      if (globalScale.toInt >= 200000) globalScale -= 100000
      else if (globalScale.toInt >= 20000) globalScale -= 10000
      else if (globalScale.toInt >= 2000) globalScale -= 1000
      else if (globalScale.toInt >= 200) globalScale -= 100
      else if (globalScale.toInt >= 20) globalScale -= 10
      else if (globalScale.toInt >= 2) globalScale -= 1
      else if ((globalScale * 10).toInt > 1) globalScale -= 0.1
      else globalScale -= 0.01
      if (globalScale < 0.01) globalScale = 0.01
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    val _maxGlobalScale = if (!orbitalComponents.drawMapMode) 30 else 1000000
    if (globalScale < _maxGlobalScale) {
      if (globalScale < 0.1) globalScale += 0.01
      else if (globalScale < 1) globalScale += 0.1
      else if (globalScale < 10) globalScale += 1
      else if (globalScale < 100) globalScale += 10
      else if (globalScale < 1000) globalScale += 100
      else if (globalScale < 10000) globalScale += 1000
      else if (globalScale < 100000) globalScale += 10000
      else globalScale += 100000
      if (globalScale > _maxGlobalScale) globalScale = _maxGlobalScale
    }
    println(globalScale)
  })


  var left_up_corner: Option[DVec] = None
  var right_down_corner: Option[DVec] = None
  var set_stop_moment = false
  leftMouseIgnorePause(onBtnDown = m => {
    if (orbitalComponents.drawMapMode) {
      left_up_corner = Some(absCoord(m))
    } else {
      InterfaceHolder.clickInterfaceElem(m, 0)
    }
  }, onBtnUp = m => {
    if (orbitalComponents.drawMapMode) {
      if (right_down_corner.nonEmpty) {
        for {
          x <- left_up_corner
          y <- right_down_corner
          c = (y - x).n * (y.dist(x) / 2f) + x
          h = math.abs(y.y - x.y)
          if h * globalScale > 10
        } {
          globalScale = math.min(1000000, 750 / h)
          if (orbitalComponents.viewMode == FixedOnOrbit) {
            orbitalComponents._center = {
              val player_ship: Ship4 = orbitalComponents.shipComponents.player_ship
              val currentPlanetStates = orbitalComponents.planetComponents.currentPlanetStates
              c - orbitAroundCelestialInPointWithVelocity(player_ship.coord, player_ship.linearVelocity, player_ship.mass, currentPlanetStates)
                .map(_._2.center * scale).getOrElse(player_ship.coord)
            }
          } else {
            orbitalComponents._center = c
          }
        }
      } else {
        val player_ship: Ship4 = orbitalComponents.shipComponents.player_ship
        if (!InterfaceHolder.clickInterfaceElem(m, 0) && player_ship.isAlive && (keyPressed(KEY_LSHIFT) || keyPressed(KEY_RSHIFT))) {
          set_stop_moment = true
        }
      }
      left_up_corner = None
      right_down_corner = None
    }
  })
  leftMouseDragIgnorePause(onDrag = m => if (orbitalComponents.drawMapMode) {
    right_down_corner = Some(absCoord(m))
  })

  rightMouseIgnorePause(onBtnDown = m => {
    if (!InterfaceHolder.clickInterfaceElem(m, 1) && (keyPressed(KEY_LSHIFT) || keyPressed(KEY_RSHIFT))) {
      orbitalComponents._stop_after_number_of_tacts = 0
    }
  })
}
