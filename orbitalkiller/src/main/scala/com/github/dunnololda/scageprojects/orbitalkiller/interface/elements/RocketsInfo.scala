package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.AdditionalSymbols

class RocketsInfo(playerShip: Ship4) extends InterfaceElement {
  private val _strings: Array[String] = Array(AdditionalSymbols.rocket_symbol.toString)
  private var _color = ScageColor.DARK_GRAY

  override def shortDescr: String = "R"

  override def data: Seq[String] = _strings

  override protected def _update(): Unit = {
    if (playerShip.rockets_enabled) {
      _color = ScageColor.YELLOW
      _strings(0) = s"Ракетное вооружение активировано. ${playerShip.rocketsStateStr}"
    } else {
      _color = ScageColor.DARK_GRAY
      _strings(0) = s"Ракетное вооружение деактивировано"
    }
  }

  override def color: ScageColor = _color
}
