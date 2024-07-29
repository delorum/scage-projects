package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.planets.{Planet, PlanetWithAir, Star}

/**
  * Created by andrey on 1/7/18.
  */
trait PlanetsAware {
  def sun: Star

  def earth: PlanetWithAir

  def moon: Planet
}
