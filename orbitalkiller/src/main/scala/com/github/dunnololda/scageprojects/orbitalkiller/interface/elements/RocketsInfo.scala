package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceElement}

class RocketsInfo extends InterfaceElement {
  private val _strings:Array[String] = Array("\u21e7")
  private var _color = ScageColor.DARK_GRAY

  override def shortDescr: String = "R"

  override def data: Seq[String] = _strings

  override protected def _update(): Unit = {
    if(OrbitalKiller.player_ship.rockets_enabled) {
      _color = ScageColor.YELLOW
      _strings(0) = s"Ракетное вооружение активировано. ${OrbitalKiller.player_ship.rocketsStateStr}"
    } else {
      _color = ScageColor.DARK_GRAY
      _strings(0) = s"Ракетное вооружение деактивировано"
    }
  }



  override def color:ScageColor = _color
}
