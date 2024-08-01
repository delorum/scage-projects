package com.github.dunnololda.scageprojects.orbitalkiller_cake

import com.github.dunnololda.scage.ScageLibD.{property, ScageScreenAppD}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.OrbitalKillerComponents

object Main extends ScageScreenAppD("Orbital Killer", property("screen.width", 1600), property("screen.height", 900)) {
  private val components = new OrbitalKillerComponents(this)
}
