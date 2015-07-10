package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller._
import OrbitalKiller._

class LinearVelocityInfo extends InterfaceElement {
  private val strings = Array(f"Линейная скорость: ${msecOrKmsec(ship.linearVelocity.norma)}")
  override protected def _update(): Unit = {
    strings(0) = f"Линейная скорость: ${msecOrKmsec(ship.linearVelocity.norma)}"
  }

  override def _data: Seq[String] = strings
}
