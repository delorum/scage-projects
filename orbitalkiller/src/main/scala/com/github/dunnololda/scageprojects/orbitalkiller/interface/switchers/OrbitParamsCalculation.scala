package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

class OrbitParamsCalculation extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("OPon", "OPoff")
  selected_variant = 1
}
