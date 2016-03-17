package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceElement}

class ViewModeInfo extends InterfaceElement {
  private val strings = Array("")
  override protected def _update(): Unit = {
    strings(0) = s"Режим камеры: ${OrbitalKiller.viewModeStr}"
  }
  override def data: Seq[String] = strings

  override val shortDescr: String = "Vm"
}
