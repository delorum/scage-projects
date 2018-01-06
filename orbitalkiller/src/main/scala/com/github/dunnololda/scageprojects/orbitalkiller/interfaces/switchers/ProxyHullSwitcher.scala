package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.interfaces.InterfaceSwitcher

class ProxyHullSwitcher extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("Ship", "Hull", "Conv", "Wreck")

  selected_variant = 0
}
