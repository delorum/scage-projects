package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers.DegOrKm
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.correctAngle
class SunRelativeInfo(degOrKm: DegOrKm) extends InterfaceElement {
  private val strings = Array("")

  override protected def _update() {
    val ship_sun_vertical_speed = msecOrKmsecOrKmhour((player_ship.linearVelocity - sun.linearVelocity) * (player_ship.coord - sun.coord).n)
    val ship_sun_tangent_speed = msecOrKmsecOrKmhour(((player_ship.linearVelocity - sun.linearVelocity) * (player_ship.coord - sun.coord).p) / player_ship.coord.dist(sun.coord) * sun.radius - sun.groundSpeedMsec)
    //val ship_earth_angular_speed = f"${(ship.linearVelocity - sun.linearVelocity)*(ship.coord - sun.coord).p/ship.coord.dist(sun.coord) - sun.currentState.ang_vel}%.3f град/сек"
    val ship_sun_position = if (degOrKm.selectedVariant == 0) {
      f"${correctAngle(DVec(0, 1).deg360(player_ship.coord - sun.coord) - sun.currentState.ang)}%.3f град."
    } else {
      val km = (correctAngle(DVec(0, 1).deg360(player_ship.coord - sun.coord) - sun.currentState.ang) / 360.0 * sun.length) / 1000
      f"$km%.2f/${sun.length/1000}%.2f км"
    }
    strings(0) = s"Солнце: dist=${mOrKmOrMKm(player_ship.coord.dist(sun.coord) - sun.radius)}, v.vel=$ship_sun_vertical_speed, h.vel=$ship_sun_tangent_speed, pos=$ship_sun_position"
  }

  override def data: Seq[String] = strings

  override val color = ScageColor.YELLOW

  override val shortDescr: String = "Su"
}
