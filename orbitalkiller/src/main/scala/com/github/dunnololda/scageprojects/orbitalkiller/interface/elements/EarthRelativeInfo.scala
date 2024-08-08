package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers.DegOrKm
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.PlanetWithAir
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.{correctAngle, MyVec}

class EarthRelativeInfo(degOrKm: DegOrKm, playerShip: Ship4, earth: PlanetWithAir) extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    // val ship_earth_angular_speed = f"${(ship.linearVelocity - earth.linearVelocity)*(ship.coord - earth.coord).p/ship.coord.dist(earth.coord) - earth.currentState.ang_vel}%.3f град/сек"
    val ship_earth_position = if (degOrKm.selectedVariant == 0) {
      f"${correctAngle(DVec(0, 1).deg360(playerShip.coord - earth.coord) - earth.currentState.ang)}%.3f град."
    } else {
      val km = (correctAngle(
        DVec(0, 1).deg360(playerShip.coord - earth.coord) - earth.currentState.ang
      ) / 360.0 * earth.length) / 1000
      f"$km%.2f/${earth.length / 1000}%.2f км"
    }
    if (playerShip.isLandedOnEarth) {
      strings(0) = s"Земля: landed, pos=$ship_earth_position"
    } else {
      val ship_earth_vertical_speed_str = msecOrKmsecOrKmhour(
        (playerShip.linearVelocity - earth.linearVelocity) * (playerShip.coord - earth.coord).n
      )
      val ship_earth_tangent_speed_str = msecOrKmsecOrKmhour(
        ((playerShip.linearVelocity - earth.linearVelocity) * (playerShip.coord - earth.coord).p) / playerShip.coord
          .dist(earth.coord) * earth.radius - earth.groundSpeedMsec
      )
      strings(0) =
        s"Земля: dist=${mOrKmOrMKm(playerShip.coord.dist(earth.coord) - earth.radius)}, v.vel=$ship_earth_vertical_speed_str, h.vel=$ship_earth_tangent_speed_str, pos=$ship_earth_position"
    }
  }

  override def data: Seq[String] = strings

  override val color: ScageColor = ScageColor.ORANGE

  override val shortDescr: String = "E"
}
