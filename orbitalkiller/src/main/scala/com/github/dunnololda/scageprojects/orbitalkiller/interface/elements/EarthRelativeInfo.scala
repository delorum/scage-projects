package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import OrbitalKiller._

class EarthRelativeInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update() {
    //val ship_earth_angular_speed = f"${(ship.linearVelocity - earth.linearVelocity)*(ship.coord - earth.coord).p/ship.coord.dist(earth.coord) - earth.currentState.ang_vel}%.3f град/сек"
    val ship_earth_vertical_speed = (ship.linearVelocity - earth.linearVelocity) * (ship.coord - earth.coord).n
    val ship_earth_tangent_speed = ((ship.linearVelocity - earth.linearVelocity) * (ship.coord - earth.coord).p) / ship.coord.dist(earth.coord) * earth.radius - earth.groundSpeedMsec
    val ship_earth_position = f"${correctAngle(DVec(0, 1).deg360(ship.coord - earth.coord) - earth.currentState.ang)}%.3f град."
    if((ship.coord.dist(earth.coord) - earth.radius) < ship.radius && ship_earth_vertical_speed.abs < 0.5 && ship_earth_tangent_speed.abs < 0.5) {
      strings(0) = s"Земля: dist=${mOrKmOrMKm(ship.coord.dist(earth.coord) - earth.radius)}, landed, pos=$ship_earth_position"
    } else {
      val ship_earth_vertical_speed_str = msecOrKmsec(ship_earth_vertical_speed)
      val ship_earth_tangent_speed_str = msecOrKmsec(ship_earth_tangent_speed)
      strings(0) = s"Земля: dist=${mOrKmOrMKm(ship.coord.dist(earth.coord) - earth.radius)}, v.vel=$ship_earth_vertical_speed_str, h.vel=$ship_earth_tangent_speed_str, pos=$ship_earth_position"
    }
  }

  override def data: Seq[String] = strings
  override val color = ScageColor.ORANGE

  override val shortDescr: String = "E"
}
