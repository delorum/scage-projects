package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics

import com.github.dunnololda.scage.ScageLibD.DVec

object TorqueUtils {
  /**
   *
   * @param force - вектор силы
   * @param force_position_from_mass_center - точка приложения силы относительно центра масс
   * @param sin_angle - синус угла между вектором от центра масс до точки приложения силы и вектором силы
   * @return
   */
  private def torque(force: DVec, force_position_from_mass_center: DVec, sin_angle: Double): Double = {
    force.norma * force_position_from_mass_center.norma * sin_angle
  }

  def torque(force: DVec, force_position_from_mass_center: DVec): Double = {
    val xf = force_position_from_mass_center * force.p
    val sin_angle = xf / force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }

  def torque(force: DVec, force_position: DVec, center: DVec): Double = {
    val force_position_from_mass_center = force_position - center
    val xf = force_position_from_mass_center * force.p
    val sin_angle = xf / force_position_from_mass_center.norma
    torque(force, force_position_from_mass_center, sin_angle)
  }
}
