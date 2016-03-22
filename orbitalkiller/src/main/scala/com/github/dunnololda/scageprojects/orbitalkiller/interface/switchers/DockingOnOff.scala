package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

class DockingOnOff extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("Doff", "Don")
  def dockingEnabled = selected_variant == 1
}
