package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.Celestials._
class PlanetsInfluenceInfo extends InterfaceElement {
  private val strings = Array("")

  override protected def _update(): Unit = {
    val sun_force = gravityForce(sun.coord, sun.mass, player_ship.coord, player_ship.mass, G).norma
    val earth_force = gravityForce(earth.coord, earth.mass, player_ship.coord, player_ship.mass, G).norma
    val moon_force = gravityForce(moon.coord, moon.mass, player_ship.coord, player_ship.mass, G).norma
    val sum_force = sun_force + earth_force + moon_force
    val sun_influence_str = f"${sun.name} ${sun_force / sum_force * 100}%.2f%%"
    val earth_influence_str = f"${earth.name} ${earth_force / sum_force * 100}%.2f%%"
    val moon_influence_str = f"${moon.name} ${moon_force / sum_force * 100}%.2f%%"
    strings(0) = f"Влияние небесных тел $sun_influence_str $earth_influence_str $moon_influence_str"
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "PI"
}
