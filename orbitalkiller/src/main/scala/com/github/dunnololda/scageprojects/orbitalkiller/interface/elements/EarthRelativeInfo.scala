package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class EarthRelativeInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update() {
    //val ship_earth_angular_speed = f"${(ship.linearVelocity - earth.linearVelocity)*(ship.coord - earth.coord).p/ship.coord.dist(earth.coord) - earth.currentState.ang_vel}%.3f град/сек"
    val ship_earth_position = if (InterfaceHolder.degOrKm.selectedVariant == 0) {
      f"${correctAngle(DVec(0, 1).deg360(player_ship.coord - earth.coord) - earth.currentState.ang)}%.3f град."
    } else {
      f"${(correctAngle(DVec(0, 1).deg360(player_ship.coord - earth.coord) - earth.currentState.ang) / 360.0 * earth.length) / 1000}%.2f км"
    }
    if (player_ship.isLandedOnPlanet(earth)) {
      strings(0) = s"Земля: landed, pos=$ship_earth_position"
    } else {
      val ship_earth_vertical_speed_str = msecOrKmsec((player_ship.linearVelocity - earth.linearVelocity) * (player_ship.coord - earth.coord).n)
      val ship_earth_tangent_speed_str = msecOrKmsec(((player_ship.linearVelocity - earth.linearVelocity) * (player_ship.coord - earth.coord).p) / player_ship.coord.dist(earth.coord) * earth.radius - earth.groundSpeedMsec)
      strings(0) = s"Земля: dist=${mOrKmOrMKm(player_ship.coord.dist(earth.coord) - earth.radius)}, v.vel=$ship_earth_vertical_speed_str, h.vel=$ship_earth_tangent_speed_str, pos=$ship_earth_position"
    }
  }

  override def data: Seq[String] = strings

  override val color = ScageColor.ORANGE

  override val shortDescr: String = "E"
}
