package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class PlanetsInfluenceInfo extends InterfaceElement {
  private val strings = Array("")
  override protected def _update(): Unit = {
    val earth_force = gravityForce(earth.coord, earth.mass, ship.coord, ship.mass, G).norma
    val moon_force = gravityForce(moon.coord, moon.mass, ship.coord, ship.mass, G).norma
    strings(0) = f"Влияние планет: Земля ${earth_force/(earth_force + moon_force)*100}%.2f%% Луна ${moon_force/(earth_force + moon_force)*100}%.2f%% З/Л ${earth_force/moon_force}%.2f Л/З ${moon_force/earth_force}%.2f"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "PI"
}
