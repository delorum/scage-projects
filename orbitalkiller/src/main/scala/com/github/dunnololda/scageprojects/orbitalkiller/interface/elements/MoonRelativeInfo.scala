package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers.DegOrKm
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.Planet
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.{correctAngle, MyVec}

class MoonRelativeInfo(degOrKm: DegOrKm, playerShip: Ship4, moon: Planet) extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    // val ship_moon_angular_speed = f"${(ship.linearVelocity - moon.linearVelocity)*(ship.coord - moon.coord).p/ship.coord.dist(moon.coord) - moon.currentState.ang_vel}%.3f град/сек"
    val ship_moon_position = if (degOrKm.selectedVariant == 0) {
      f"${correctAngle(DVec(0, 1).deg360(playerShip.coord - moon.coord) - moon.currentState.ang)}%.3f град."
    } else {
      val km = (correctAngle(
        DVec(0, 1).deg360(playerShip.coord - moon.coord) - moon.currentState.ang
      ) / 360.0 * moon.length) / 1000
      f"$km%.2f/${moon.length / 1000}%.2f км"
    }
    if (playerShip.isLandedOnMoon) {
      strings(0) = s"Луна: landed, pos=$ship_moon_position"
    } else {
      val ship_moon_vertical_speed_str = msecOrKmsecOrKmhour(
        (playerShip.linearVelocity - moon.linearVelocity) * (playerShip.coord - moon.coord).n
      )
      val ship_moon_tangent_speed_str = msecOrKmsecOrKmhour(
        ((playerShip.linearVelocity - moon.linearVelocity) * (playerShip.coord - moon.coord).p) / playerShip.coord.dist(
          moon.coord
        ) * moon.radius - moon.groundSpeedMsec
      )
      strings(0) =
        s"Луна: dist=${mOrKmOrMKm(playerShip.coord.dist(moon.coord) - moon.radius)}, v.vel=$ship_moon_vertical_speed_str, h.vel=$ship_moon_tangent_speed_str, pos=$ship_moon_position"
    }
  }

  override def data: Seq[String] = strings

  override val color: ScageColor = ScageColor.GREEN

  override val shortDescr: String = "M"
}
