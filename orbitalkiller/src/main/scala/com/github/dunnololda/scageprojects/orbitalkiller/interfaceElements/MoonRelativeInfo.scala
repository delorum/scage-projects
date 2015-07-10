package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class MoonRelativeInfo extends InterfaceElement {
  private val strings = Array("Расстояние, скорость и позиция относительно Луны:", "")

  override protected def _update(): Unit = {
    val ship_moon_vertical_speed = msecOrKmsec((ship.linearVelocity - moon.linearVelocity) * (ship.coord - moon.coord).n)
    val ship_moon_tangent_speed = msecOrKmsec(((ship.linearVelocity - moon.linearVelocity)*(ship.coord - moon.coord).p)/ship.coord.dist(moon.coord)*moon.radius - moon.groundSpeedMsec)
    //val ship_moon_angular_speed = f"${(ship.linearVelocity - moon.linearVelocity)*(ship.coord - moon.coord).p/ship.coord.dist(moon.coord) - moon.currentState.ang_vel}%.3f град/сек"
    val ship_moon_position = f"${correctAngle((ship.coord - moon.coord).deg360(Vec(0, 1)) - moon.currentState.ang)}%.3f град."
    strings(1) = s"${mOrKm(ship.coord.dist(moon.coord) - moon.radius)}, $ship_moon_vertical_speed, $ship_moon_tangent_speed, $ship_moon_position"
  }

  override protected def _data: Seq[String] = strings
}
