package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller._
import OrbitalKiller._

class NearestShipInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    val other_ship_near = OrbitalKiller.ship.otherShipsNear.headOption
    other_ship_near match {
      case Some(os) =>
        strings(0) = f"Расстояние и скорость относительно ближайшего корабля: ${mOrKm(ship.coord.dist(os.coord))}, ${msecOrKmsec((ship.linearVelocity - os.linearVelocity)* (ship.coord - os.coord).n)}"
      case None =>
        strings(0) = ""
    }
  }
  override def data: Seq[String] = strings

  override val shortDescr: String = "NS"
}
