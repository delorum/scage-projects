package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

class ShipParamsWhenEnginesOff extends InterfaceElement {
  private val strings = Array("", "", "")
  override protected def _update(): Unit = {
    if (ship.flightMode != 0) {
      val engines_active = ship.engines.exists(_.active)
      if(engines_active) {
        strings(0) = f"Линейная скорость в момент отключения двигателей: $linearSpeedStrWhenEnginesOff"
        strings(1) = f"Угловая скорость в момент отключения двигателей: $angularSpeedStrWhenEnginesOff"
        strings(2) = s"Параметры орбиты в момент отключения двигателей: $orbitParametersStrWhenEnginesOff"
      }
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "SP"
}
