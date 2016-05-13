package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

class NamesOnOff extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("Non", "Noff")

  selected_variant = 1

  def showNames: Boolean = selectedVariant == 0
}
