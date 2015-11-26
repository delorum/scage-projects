package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import OrbitalKiller._

class EarthRelativeInfo extends InterfaceElement {
  private val strings = Array("Расстояние, скорость и позиция относительно Земли:", "")

  override protected def _update() {
    val ship_earth_vertical_speed = msecOrKmsec((ship.linearVelocity - earth.linearVelocity) * (ship.coord - earth.coord).n)
    val ship_earth_tangent_speed = msecOrKmsec(((ship.linearVelocity - earth.linearVelocity) * (ship.coord - earth.coord).p) / ship.coord.dist(earth.coord) * earth.radius - earth.groundSpeedMsec)
    //val ship_earth_angular_speed = f"${(ship.linearVelocity - earth.linearVelocity)*(ship.coord - earth.coord).p/ship.coord.dist(earth.coord) - earth.currentState.ang_vel}%.3f град/сек"
    val ship_earth_position = f"${correctAngle(DVec(0, 1).deg360(ship.coord - earth.coord) - earth.currentState.ang)}%.3f град."
    strings(1) = s"${mOrKm(ship.coord.dist(earth.coord) - earth.radius)}, $ship_earth_vertical_speed, $ship_earth_tangent_speed, $ship_earth_position"
  }

  override def data: Seq[String] = strings
  override val color = ScageColor.ORANGE

  override val shortDescr: String = "E"
}
