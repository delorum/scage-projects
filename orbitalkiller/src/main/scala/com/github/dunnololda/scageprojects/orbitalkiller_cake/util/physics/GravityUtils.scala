package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState

object GravityUtils {
  def gravityForce(body1_coord: DVec, body1_mass: Double, body2_coord: DVec, body2_mass: Double, G: Double): DVec = {
    (body1_coord - body2_coord).n * G * body1_mass * body2_mass / body1_coord.dist2(body2_coord)
  }

  def equalGravityRadius(planet1: MutableBodyState, planet2: MutableBodyState): Double = {
    val A = planet1.coord.dist(planet2.coord)
    val X = planet1.mass / planet2.mass
    A * math.sqrt(X) / (math.sqrt(X) + 1)
  }

  /**
   * soi - sphere of influence. Радиус сферы вокруг планеты, в которой она оказывает наибольшее гравитационное влияние
   * @param smaller_planet_mass - масса малой планеты
   * @param semi_major_axis - главная полуось орбиты малой планеты
   * @param bigger_planet_mass - масса большей планеты или звезды, вокруг которой вращается малая планета
   * @return
   */
  def soi(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    semi_major_axis * math.pow(smaller_planet_mass / bigger_planet_mass, 2.0 / 5)
  }

  def halfHillRadius(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    0.5 * semi_major_axis * math.pow(smaller_planet_mass / (3 * bigger_planet_mass), 1.0 / 3)
  }
}
