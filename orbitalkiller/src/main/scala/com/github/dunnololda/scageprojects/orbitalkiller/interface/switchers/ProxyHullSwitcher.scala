package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

class ProxyHullSwitcher extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("Ship", "Hull", "Conv", "Wreck")

  selected_variant = 0
}
