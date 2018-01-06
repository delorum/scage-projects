package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.interfaces.InterfaceSwitcher
import com.github.dunnololda.scageprojects.orbitalkiller.{CelestialBody, OrbitalKiller, RealTrajectory}

class OrbitSwitcher extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("Auto", "Earth", "Moon", "Sun")

  def calculateOrbitAround:Option[Int] = selected_variant match {
    case 0 => None
    case 1 => Some(OrbitalKiller.earth.index)
    case 2 => Some(OrbitalKiller.moon.index)
    case 3 => Some(OrbitalKiller.sun.index)
    case _ => None
  }

  def calculateOrbitAroundPlanet:Option[CelestialBody] = selected_variant match {
    case 0 => None
    case 1 => Some(OrbitalKiller.earth)
    case 2 => Some(OrbitalKiller.moon)
    case 3 => Some(OrbitalKiller.sun)
    case _ => None
  }

  override def switchForward(): Unit = {
    super.switchForward()
    OrbitalKiller.needToUpdateOrbits("changed central body")
  }

  override def switchBack(): Unit = {
    super.switchBack()
    OrbitalKiller.needToUpdateOrbits("changed central body")
  }
}
