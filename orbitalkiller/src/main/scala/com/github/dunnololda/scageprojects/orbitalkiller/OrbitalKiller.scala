package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD.{property, _}
import com.github.dunnololda.scageprojects.orbitalkiller.components.OrbitalComponents

object OrbitalKiller
  extends ScageScreenAppDMT("Orbital Killer", property("screen.width", 1280), property("screen.height", 768))
    with OrbitKeyControls with OrbitMouseControls with OrbitAction with OrbitRender {
  val orbitalComponents: OrbitalComponents = new OrbitalComponents

  pause()
}






