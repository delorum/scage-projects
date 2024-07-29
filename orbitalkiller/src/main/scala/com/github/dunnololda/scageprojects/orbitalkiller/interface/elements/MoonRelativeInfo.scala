package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.{InterfaceElement, InterfaceHolder}

class MoonRelativeInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    //val ship_moon_angular_speed = f"${(ship.linearVelocity - moon.linearVelocity)*(ship.coord - moon.coord).p/ship.coord.dist(moon.coord) - moon.currentState.ang_vel}%.3f град/сек"
    val ship_moon_position = if (InterfaceHolder.degOrKm.selectedVariant == 0) {
      f"${correctAngle(DVec(0, 1).deg360(player_ship.coord - moon.coord) - moon.currentState.ang)}%.3f град."
    } else {
      val km = (correctAngle(DVec(0, 1).deg360(player_ship.coord - moon.coord) - moon.currentState.ang) / 360.0 * moon.length) / 1000
      f"$km%.2f/${moon.length/1000}%.2f км"
    }
    if (player_ship.isLandedOnMoon) {
      strings(0) = s"Луна: landed, pos=$ship_moon_position"
    } else {
      val ship_moon_vertical_speed_str = msecOrKmsec((player_ship.linearVelocity - moon.linearVelocity) * (player_ship.coord - moon.coord).n)
      val ship_moon_tangent_speed_str = msecOrKmsec(((player_ship.linearVelocity - moon.linearVelocity) * (player_ship.coord - moon.coord).p) / player_ship.coord.dist(moon.coord) * moon.radius - moon.groundSpeedMsec)
      strings(0) = s"Луна: dist=${mOrKmOrMKm(player_ship.coord.dist(moon.coord) - moon.radius)}, v.vel=$ship_moon_vertical_speed_str, h.vel=$ship_moon_tangent_speed_str, pos=$ship_moon_position"
    }
  }

  override def data: Seq[String] = strings

  override val color = ScageColor.GREEN

  override val shortDescr: String = "M"
}
