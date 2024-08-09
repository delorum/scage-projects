package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.CelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.orbits.KeplerOrbit.calculateOrbit
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics.GravityUtils.{
  insideSphereOfInfluenceOfCelestialBody,
  G
}

object OrbitUtils {

  private def _satelliteSpeed(
      from_planet_to_body: DVec,
      planet_velocity: DVec,
      planet_mass: Double,
      counterclockwise: Boolean): DVec = {
    val sat_speed = from_planet_to_body.p * math.sqrt(G * planet_mass / from_planet_to_body.norma)
    if (!counterclockwise) planet_velocity + sat_speed * -1
    else planet_velocity + sat_speed
  }

  def satelliteSpeed(
      body_coord: DVec,
      planet_coord: DVec,
      planet_velocity: DVec,
      planet_mass: Double,
      counterclockwise: Boolean): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, counterclockwise)
  }

  def satelliteSpeed(
      body_coord: DVec,
      body_velocity: DVec,
      planet_coord: DVec,
      planet_velocity: DVec,
      planet_mass: Double): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular * (body_velocity - planet_velocity) >= 0
    _satelliteSpeed(from_planet_to_body, planet_velocity, planet_mass, counterclockwise)
  }

  private def _escapeVelocity(
      from_planet_to_body: DVec,
      planet_velocity: DVec,
      planet_mass: Double,
      counterclockwise: Boolean): DVec = {
    val esc_speed = from_planet_to_body.p * math.sqrt(G * planet_mass / from_planet_to_body.norma) * math.sqrt(2)
    if (!counterclockwise) planet_velocity + esc_speed * -1
    else planet_velocity + esc_speed
  }

  def escapeVelocity(
      body_coord: DVec,
      planet_coord: DVec,
      planet_velocity: DVec,
      planet_mass: Double,
      counterclockwise: Boolean): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, counterclockwise)
  }

  def escapeVelocity(
      body_coord: DVec,
      body_velocity: DVec,
      planet_coord: DVec,
      planet_velocity: DVec,
      planet_mass: Double): DVec = {
    val from_planet_to_body = body_coord - planet_coord
    val counterclockwise = from_planet_to_body.perpendicular * (body_velocity - planet_velocity) >= 0
    _escapeVelocity(from_planet_to_body, planet_velocity, planet_mass, counterclockwise)
  }

  /**
   * Возвращает скорость, которую надо иметь кораблю, чтобы быть в перигее или апогее орбиты с заданными параметрами
   * @param perigee_coord - текущая координата корабля, и это будет координата перигея или апогея
   * @param apogee_diff - возвышение противоположной точки: насколько апогей выше перигея (если мы в перигее), либо наоборот, насколько перигей ниже,
   *                    положительное или отрицательное число тут
   * @param planet_coord - координата планеты
   * @param planet_velocity - скорость планеты
   * @param planet_mass - масса планеты
   * @return двумерный вектор скорости
   */
  def speedToHaveOrbitWithParams(
      perigee_coord: DVec,
      apogee_diff: Double,
      planet_coord: DVec,
      planet_velocity: DVec,
      planet_mass: Double,
      ccw: Boolean = true): DVec = {
    val r_p = perigee_coord.dist(planet_coord)
    val r_a = r_p + apogee_diff
    val mu = planet_mass * G
    if (ccw) {
      planet_velocity + math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    } else {
      planet_velocity - math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    }
  }

  def specificOrbitalEnergy(
      planet_mass: Double,
      body_mass: Double,
      body_relative_coord: DVec,
      body_relative_velocity: DVec): Double = {
    val mu = (planet_mass + body_mass) * G // гравитационный параметр
    body_relative_velocity.norma2 / 2 - mu / body_relative_coord.norma
  }

  def orbitStrInPointWithVelocity(
      coord: DVec,
      velocity: DVec,
      radius: Double,
      mass: Double,
      planet_states: Seq[(CelestialBody, MutableBodyState)]): String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, planet_states) match {
      case Some((planet, planet_state)) =>
        val orbit = calculateOrbit(
          planet_state.mass,
          planet_state.coord,
          mass,
          coord - planet_state.coord,
          velocity - planet_state.vel
        )
        orbit.strDefinition(
          planet.name,
          planet.radius,
          planet_state.vel,
          planet_state.ang,
          planet.groundSpeedMsec,
          planet.g,
          coord,
          velocity,
          radius
        )
      case None => "N/A"
    }
  }
}
