package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

class OrbitParamsCalculation extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("OPon", "OPoff")

  selected_variant = 1

  def calculationOn = selected_variant == 0

  def calculationOff = selected_variant == 1
}
