package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.{CelestialBody, Planet, PlanetWithAir, Star}

class OrbitSwitcher(sun: Star, earth: PlanetWithAir, moon: Planet) extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("Auto", "Earth", "Moon", "Sun")

  def calculateOrbitAround: Option[Int] = selected_variant match {
    case 0 => None
    case 1 => Some(earth.index)
    case 2 => Some(moon.index)
    case 3 => Some(sun.index)
    case _ => None
  }

  def calculateOrbitAroundPlanet: Option[CelestialBody] = selected_variant match {
    case 0 => None
    case 1 => Some(earth)
    case 2 => Some(moon)
    case 3 => Some(sun)
    case _ => None
  }

  override def switchForward(): Unit = {
    super.switchForward()
    Main.needToUpdateRealTrajectory("changed central body")
  }

  override def switchBack(): Unit = {
    super.switchBack()
    Main.needToUpdateRealTrajectory("changed central body")
  }
}
