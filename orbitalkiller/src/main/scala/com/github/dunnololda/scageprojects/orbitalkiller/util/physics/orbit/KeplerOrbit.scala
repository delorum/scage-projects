package com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.MathUtils._

trait KeplerOrbit {
  def a: Double

  def b: Double

  def e: Double

  def f: DVec

  def center: DVec

  def r_p: Double

  protected def calcFallPos(fall_point: DVec, planet_ang: Double, planet_radius: Double): String = {
    if (InterfaceHolder.degOrKm.selectedVariant == 0) {
      f"${correctAngle(DVec(0, 1).deg360(fall_point - f) - planet_ang)}%.3f град."
    } else {
      val planet_length = 2 * math.Pi * planet_radius
      val km = (correctAngle(DVec(0, 1).deg360(fall_point - f) - planet_ang) / 360.0 * planet_length) / 1000
      f"$km%.2f/${planet_length / 1000}%.2f км"
    }
  }

  def strDefinition(prefix: String,
                    planet_radius: Double,
                    planet_velocity: DVec,
                    planet_ang: Double,
                    planet_groundSpeedMsec: Double,
                    planet_g: Double,
                    ship_coord: DVec,
                    ship_velocity: DVec,
                    ship_radius: Double): String

  def orbitalPointByTrueAnomalyDeg(angle_deg: Double): DVec

  def tetaRad2PiInPoint(p: DVec): Double

  def distanceByTrueAnomalyRad(angle_rad: Double): Double

  def orbitalVelocityValueByTrueAnomalyRad(teta_rad: Double): Double

  def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, ccw: Boolean): DVec

  def orbitalPointAfterTime(point1: DVec, time_msec: Long, ccw: Boolean): DVec

  def withNewFocusPosition(new_f: DVec): KeplerOrbit

  def orbitalPointInPoint(p: DVec): DVec

  def travelTimeOnOrbitMsec(from: DVec, to: DVec, ccw: Boolean, recalculate_orbital_points: Boolean = false): Long

  def orbitalPointAfterTimeCCW(coord: DVec, flight_time_msec: Long): DVec

  def orbitalVelocityInPoint(point: DVec, ccw: Boolean): DVec
}

object KeplerOrbit {
  /**
    * Вычисляет параметры орбиты
    *
    * @param planet_mass            - масса планеты
    * @param planet_coord           - абсолютная координата планеты
    * @param body_mass              - масса тела, вращающегося вокруг планеты
    * @param body_relative_coord    - относительная координата тела (абсолютная координата тела минус абсолютная координата планеты)
    * @param body_relative_velocity - относительная линейная скорость тела (абсолютная скорость тела минус абсолютная скорость планеты)
    * @param G                      - гравитационная постоянная
    * @return объект Orbit, содержащий вычисленный набор параметров
    */
  def calculateOrbit(planet_mass: Double, planet_coord: DVec, body_mass: Double, body_relative_coord: DVec, body_relative_velocity: DVec, G: Double): KeplerOrbit = {
    //https://ru.wikipedia.org/wiki/Гравитационный_параметр
    //val mu = (planet_mass + body_mass) * G // гравитационный параметр
    val mu = planet_mass * G // гравитационный параметр

    //http://ru.wikipedia.org/wiki/Кеплеровы_элементы_орбиты
    //val a = body_relative_coord.norma*k/(2*k*k - body_relative_coord.norma*body_velocity.norma2)

    //https://en.wikipedia.org/wiki/Specific_orbital_energy
    val epsilon = body_relative_velocity.norma2 / 2 - mu / body_relative_coord.norma // орбитальная энергия - сумма потенциальной и кинетической энергии тел, деленные на приведенную массу

    //http://ru.wikipedia.org/wiki/Большая_полуось
    val a = math.abs(mu / (2 * epsilon)) // большая полуось

    //http://en.wikipedia.org/wiki/Kepler_orbit
    val r_n = body_relative_coord.n
    val v_t = math.abs(body_relative_velocity * r_n.perpendicular)
    val p = math.pow(body_relative_coord.norma * v_t, 2) / mu // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)

    //http://ru.wikipedia.org/wiki/Эллипс
    val b = math.sqrt(math.abs(a * p)) // малая полуось

    if (epsilon < 0) {
      val e = math.sqrt(math.abs(1 - (b * b) / (a * a))) // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)

      val c = a * e // фокальное расстояние (полурасстояние между фокусами)
      val r_p = a * (1 - e) // перигей
      val r_a = a * (1 + e) // апогей

      val t = 2 * math.Pi * math.sqrt(math.abs(a * a * a / mu)) // орбитальный период (период обращения по орбите, в секундах)

      val d1 = body_relative_coord.norma // расстояние от тела до первого фокуса (планеты)
      val d2 = 2 * a - d1 // расстояние до второго фокуса (свойства эллипса: d1+d2 = 2*a)
      val alpha = body_relative_coord.signedDeg(body_relative_velocity) // угол между вектором скорости тела - касательным к эллипсу и направлением на первый фокус (свойство эллипса: угол между касательной и вектором на второй фокус такой же)
      val f1 = planet_coord // координаты первого фокуса - координаты планеты (она в фокусе эллипса-орбиты)
      val f2 = body_relative_velocity.rotateDeg(alpha).n * d2 + body_relative_coord + planet_coord // координаты второго фокуса
      val center = (f2 - f1).n * c + f1 // координаты центра орбиты-эллипса

      /*if(a == d1 || f2 == f1) {
        println("orbit is round")
      }

      if(a == 0 || b == 0 || e.isNaN || c.isNaN || p == 0 || r_p.isNaN || r_a.isNaN || t == 0) {
        throw new Exception("ellipse orbit calculation failed")
      }*/

      new EllipseOrbit(a, b, e, c, p, r_p, r_a, t, f1, f2, center, G * (planet_mass + body_mass))
    } else {
      // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
      val e = math.sqrt(math.abs(1 + (b * b) / (a * a)))

      val true_anomaly = math.abs(((a * (e * e - 1) - body_relative_coord.norma) / (body_relative_coord.norma * e)).myacos)

      val counterclockwise = body_relative_coord.signedDeg(body_relative_velocity) > 0
      val moving_away = body_relative_velocity * body_relative_coord.n > 0

      val signum = if (counterclockwise) {
        if (moving_away) -1 else 1
      } else {
        if (moving_away) 1 else -1
      }

      val center = planet_coord - body_relative_coord.rotateRad(true_anomaly * signum).n * a * e

      new HyperbolaOrbit(a, b, e, planet_coord, center, mu)
    }
  }

  def specificOrbitalEnergy(planet_mass: Double,
                            planet_coord: DVec,
                            body_mass: Double,
                            body_relative_coord: DVec,
                            body_relative_velocity: DVec, G: Double): Double = {
    val mu = (planet_mass + body_mass) * G // гравитационный параметр
    body_relative_velocity.norma2 / 2 - mu / body_relative_coord.norma
  }
}