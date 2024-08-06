package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials

import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.{Planet, PlanetWithAir, Star}

trait CelestialsAware {
  def sun: Star

  def earth: PlanetWithAir

  def moon: Planet
  def celestialsHelper: CelestialsHelper
}
