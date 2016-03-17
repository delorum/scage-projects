package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class MoonRelativeInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    val ship_moon_vertical_speed = msecOrKmsec((ship.linearVelocity - moon.linearVelocity) * (ship.coord - moon.coord).n)
    val ship_moon_tangent_speed = msecOrKmsec(((ship.linearVelocity - moon.linearVelocity)*(ship.coord - moon.coord).p)/ship.coord.dist(moon.coord)*moon.radius - moon.groundSpeedMsec)
    //val ship_moon_angular_speed = f"${(ship.linearVelocity - moon.linearVelocity)*(ship.coord - moon.coord).p/ship.coord.dist(moon.coord) - moon.currentState.ang_vel}%.3f град/сек"
    val ship_moon_position = f"${correctAngle(DVec(0, 1).deg360(ship.coord - moon.coord) - moon.currentState.ang)}%.3f град."
    strings(0) = s"Луна: dist=${mOrKmOrMKm(ship.coord.dist(moon.coord) - moon.radius)}, v.vel=$ship_moon_vertical_speed, h.vel=$ship_moon_tangent_speed, pos=$ship_moon_position"
  }

  override def data: Seq[String] = strings
  override val color = ScageColor.GREEN

  override val shortDescr: String = "M"
}
