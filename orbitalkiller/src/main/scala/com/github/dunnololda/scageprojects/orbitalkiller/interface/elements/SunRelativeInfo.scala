package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class SunRelativeInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update() {
    val ship_sun_vertical_speed = msecOrKmsec((ship.linearVelocity - sun.linearVelocity) * (ship.coord - sun.coord).n)
    val ship_sun_tangent_speed = msecOrKmsec(((ship.linearVelocity - sun.linearVelocity) * (ship.coord - sun.coord).p) / ship.coord.dist(sun.coord) * sun.radius - sun.groundSpeedMsec)
    //val ship_earth_angular_speed = f"${(ship.linearVelocity - sun.linearVelocity)*(ship.coord - sun.coord).p/ship.coord.dist(sun.coord) - sun.currentState.ang_vel}%.3f град/сек"
    val ship_sun_position = f"${correctAngle(DVec(0, 1).deg360(ship.coord - sun.coord) - sun.currentState.ang)}%.3f град."
    strings(0) = s"Солнце: dist=${mOrKmOrMKm(ship.coord.dist(sun.coord) - sun.radius)}, v.vel=$ship_sun_vertical_speed, h.vel=$ship_sun_tangent_speed, pos=$ship_sun_position"
  }

  override def data: Seq[String] = strings
  override val color = ScageColor.YELLOW

  override val shortDescr: String = "Su"
}
