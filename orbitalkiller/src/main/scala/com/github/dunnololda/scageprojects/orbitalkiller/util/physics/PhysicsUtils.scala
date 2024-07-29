package com.github.dunnololda.scageprojects.orbitalkiller.util.physics

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.MutableBodyState

/**
  * Created by andrey on 1/6/18.
  */
object PhysicsUtils {
  def gravityForce(body1_coord: DVec, body1_mass: Double, body2_coord: DVec, body2_mass: Double, G: Double): DVec = {
    (body1_coord - body2_coord).n * G * body1_mass * body2_mass / body1_coord.dist2(body2_coord)
  }

  private def _satelliteSpeed(from_planet_to_body: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val sat_speed = from_planet_to_body.p * scala.math.sqrt(G * planet_mass / from_planet_to_body.norma)
    if (!counterclockwise) planet_velocity + sat_speed * (-1)
    else planet_velocity + sat_speed
  }

  def satelliteSpeed(body_coord: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def satelliteSpeed(body_coord: DVec, body_velocity: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular * (body_velocity - planet_velocity) >= 0
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  private def _escapeVelocity(from_planet_to_body: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val esc_speed = from_planet_to_body.p * scala.math.sqrt(G * planet_mass / from_planet_to_body.norma) * scala.math.sqrt(2)
    if (!counterclockwise) planet_velocity + esc_speed * (-1)
    else planet_velocity + esc_speed
  }

  def escapeVelocity(body_coord: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  def escapeVelocity(body_coord: DVec, body_velocity: DVec, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular * (body_velocity - planet_velocity) >= 0
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, G, counterclockwise)
  }

  /**
    * Возвращает скорость, которую надо иметь кораблю, чтобы быть в перигее или апогее орбиты с заданными параметрами
    *
    * @param perigee_coord   - текущая координата корабля, и это будет координата перигея или апогея
    * @param apogee_diff     - возвышение противоположной точки: насколько апогей выше перигея (если мы в перигее), либо наоборот, насколько перигей ниже,
    *                        положительное или отрицательное число тут
    * @param planet_coord    - координата планеты
    * @param planet_velocity - скорость планеты
    * @param planet_mass     - масса планеты
    * @param G               - гравитационная постоянная
    * @return двумерный вектор скорости
    */
  def speedToHaveOrbitWithParams(perigee_coord: DVec, apogee_diff: Double, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, ccw: Boolean = true): DVec = {
    val r_p = perigee_coord.dist(planet_coord)
    val r_a = r_p + apogee_diff
    val mu = planet_mass * G
    if (ccw) {
      planet_velocity + scala.math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    } else {
      planet_velocity - scala.math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    }
  }

  def equalGravityRadius(planet1: MutableBodyState, planet2: MutableBodyState): Double = {
    val A = planet1.coord.dist(planet2.coord)
    val X = planet1.mass / planet2.mass
    A * scala.math.sqrt(X) / (scala.math.sqrt(X) + 1)
  }

  /**
    * soi - sphere of influence. Радиус сферы вокруг планеты, в которой она оказывает наибольшее гравитационное влияние
    *
    * @param smaller_planet_mass - масса малой планеты
    * @param semi_major_axis     - главная полуось орбиты малой планеты
    * @param bigger_planet_mass  - масса большей планеты или звезды, вокруг которой вращается малая планета
    * @return
    */
  def soi(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    semi_major_axis * scala.math.pow(smaller_planet_mass / bigger_planet_mass, 2.0 / 5)
  }

  def halfHillRadius(smaller_planet_mass: Double, semi_major_axis: Double, bigger_planet_mass: Double): Double = {
    0.5 * semi_major_axis * scala.math.pow(smaller_planet_mass / (3 * bigger_planet_mass), 1.0 / 3)
  }

  /**
    *
    * @param force                           - вектор силы
    * @param force_position_from_mass_center - точка приложения силы относительно центра масс
    * @param sin_angle                       - синус угла между вектором от центра масс до точки приложения силы и вектором силы
    * @return
    */
  def torque(force: DVec, force_position_from_mass_center: DVec, sin_angle: Double): Double = {
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
