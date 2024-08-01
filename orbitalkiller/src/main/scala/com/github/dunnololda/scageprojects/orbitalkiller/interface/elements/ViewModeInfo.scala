package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main

class ViewModeInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    strings(0) = s"Режим камеры: ${Main.viewMode.rusStr}"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "Vm"
}
