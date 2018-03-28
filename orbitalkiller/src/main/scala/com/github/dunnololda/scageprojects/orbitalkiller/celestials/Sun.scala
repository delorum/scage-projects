package com.github.dunnololda.scageprojects.orbitalkiller.celestials

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents.sunIndex

/**
  * TODO
  *
  * @author aborunov
  */
class Sun extends Star(
  sunIndex, "Солнце",
  mass = 1.9891E30,
  coord = DVec(0, 1.496E11),
  radius = 6.9551E8
)
