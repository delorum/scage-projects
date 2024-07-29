package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.interfaces.InterfaceSwitcher

class TimeStepSwitcher extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("15ms", "1s", "1m")

  def timeStep: Int = selected_variant match {
    case 0 => 1
    case 1 => 63
    case 2 => 3780
    case _ => 1
  }
}
