package com.github.dunnololda.scageprojects.orbitalkiller.util.physics.orbit

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller
import com.github.dunnololda.scageprojects.orbitalkiller.interface.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller.util.LogUtils
import com.github.dunnololda.scageprojects.orbitalkiller.util.StringUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.MathUtils._

class HyperbolaOrbit(
                      val a: Double, // большая полуось
                      val b: Double,
                      val e: Double, // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
                      val f: DVec, // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
                      val center: DVec, // координаты центра
                      val mu: Double) extends KeplerOrbit {
  def withNewFocusPosition(new_f: DVec): HyperbolaOrbit = {
    val new_center = center - f + new_f
    new HyperbolaOrbit(a, b, e, new_f, new_center, mu)
  }

  val r_p = a * (e - 1)
  // перигей
  val p = a * (e * e - 1) // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)

  val teta_deg_min = 180 - (1 / e).myacos / math.Pi * 180
  // разрешенные углы: от 0 до этого угла
  val teta_rad_min = teta_deg_min / 180.0 * math.Pi

  val teta_deg_max = -180 + (1 / e).myacos / math.Pi * 180 + 360
  // разрешенные углы: от этого угла до 360
  val teta_rad_max = teta_deg_max / 180.0 * math.Pi

  def tetaDeg360Valid(teta_deg360: Double): Boolean = {
    teta_deg360 <= teta_deg_min || teta_deg360 >= teta_deg_max
  }

  def tetaRad2PiValid(teta_rad2Pi: Double): Boolean = {
    teta_rad2Pi <= teta_rad_max || teta_rad2Pi >= teta_rad_min
  }

  val f_minus_center_n = (f - center).n
  val half_center = f - f_minus_center_n * (center.dist(f) * 0.5)
  val inv_n = a * math.sqrt(a / mu)

  def strDefinition(prefix: String,
                    planet_radius: Double,
                    planet_velocity: DVec,
                    planet_ang: Double,
                    planet_groundSpeedMsec: Double,
                    planet_g: Double,
                    ship_coord: DVec,
                    ship_velocity: DVec,
                    ship_radius: Double): String = {
    val ccw = (ship_coord - f).perpendicular * (ship_velocity - planet_velocity) >= 0 // летим против часовой?
    val dir = if (ccw) OrbitalKiller.ccw_symbol else OrbitalKiller.cw_symbol
    val r_p_approach = (ship_coord - f) * (ship_velocity - planet_velocity) >= 0
    val r_p_approach_str = if (r_p_approach) "удаляемся" else "приближаемся"
    if (r_p - planet_radius < 0 && !r_p_approach) {
      val y_axis = (ship_coord - f).n
      val v0y = (ship_velocity - planet_velocity) * y_axis

      /*val ship_earth_vertical_speed = (ship_velocity - planet_velocity) * (ship_coord - f).n
      val ship_earth_tangent_speed = ((ship_velocity - planet_velocity) * (ship_coord - f).p) / ship_coord.dist(f) * planet_radius - planet_groundSpeedMsec*/
      val (fall_time_msec, fall_position) = if ((ship_coord.dist(f) - planet_radius) < ship_radius /* &&
                                ship_earth_vertical_speed.abs < 0.5 &&
                                ship_earth_tangent_speed.abs < 0.5*/ ) {
        val fall_pos = calcFallPos(ship_coord, planet_ang, planet_radius)
        (0l, fall_pos)
      } else if (ccw) {
        val fall_teta_rad = -((p / (planet_radius + 3) - 1) / e).myacos + 2 * math.Pi
        val fall_point = orbitalPointByTrueAnomalyRad(fall_teta_rad)
        val fall_pos = calcFallPos(fall_point, planet_ang, planet_radius)
        (travelTimeOnOrbitMsecCCW(ship_coord, fall_point), fall_pos)
      } else {
        val fall_teta_rad = ((p / (planet_radius + 3) - 1) / e).myacos
        val fall_point = orbitalPointByTrueAnomalyRad(fall_teta_rad)
        val fall_pos = calcFallPos(fall_point, planet_ang, planet_radius)
        (travelTimeOnOrbitMsecCW(ship_coord, fall_point), fall_pos)
      }

      val allowed_acc = if (InterfaceHolder.gSwitcher.maxGSet) InterfaceHolder.gSwitcher.maxG * planet_g else 1000000 / OrbitalKiller.player_ship.mass
      val time_to_stop_at_full_power = math.abs(v0y / (allowed_acc - planet_g))
      val fall_time_str = if (fall_time_msec < 500) "" else if (fall_time_msec < 30000) s"[r Поверхность через ${timeStrSec(fall_time_msec)}, $fall_position (${timeStrMsec((time_to_stop_at_full_power * 1000l).toLong)})]" else s"Поверхность через ${timeStrMsec(fall_time_msec)}, $fall_position"
      f"$prefix, незамкнутая, суборбитальная $dir, $r_p_approach_str, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, $fall_time_str"
    } else {
      f"$prefix, незамкнутая, $dir, $r_p_approach_str, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}"
    }
  }

  def tetaDeg360ByDir(dir: DVec): Double = {
    f_minus_center_n.deg360(dir)
  }

  def tetaDeg360InPoint(p: DVec) = tetaDeg360ByDir(p - f)

  def tetaRad2PiByDir(dir: DVec): Double = {
    f_minus_center_n.rad2Pi(dir)
  }

  def tetaRad2PiInPoint(p: DVec) = tetaRad2PiByDir(p - f)

  def tetaSignedRadByDir(dir: DVec) = f_minus_center_n.signedRad(dir)

  def tetaSignedRadInPoint(p: DVec) = tetaSignedRadByDir(p - f)

  def distanceByTrueAnomalyRad(teta_rad: Double) = {
    p / (1 + e * math.cos(teta_rad))
  }

  def distanceByTrueAnomalyDeg(teta_deg: Double) = {
    p / (1 + e * math.cos(teta_deg / 180.0 * math.Pi))
  }

  def distanceByDir(dir: DVec): Double = {
    p / (1 + e * math.cos(tetaRad2PiByDir(dir)))
  }

  def distanceInPoint(point: DVec) = {
    p / (1 + e * math.cos(tetaSignedRadInPoint(point)))
  }

  def orbitalPointByTrueAnomalyRad(teta_rad: Double) = {
    f + f_minus_center_n.rotateRad(teta_rad).n * distanceByTrueAnomalyRad(teta_rad)
  }

  def orbitalPointByTrueAnomalyDeg(teta_deg: Double) = {
    f + f_minus_center_n.rotateDeg(teta_deg).n * distanceByTrueAnomalyDeg(teta_deg)
  }

  def orbitalPointByDir(dir: DVec) = {
    f + dir.n * distanceByDir(dir)
  }

  def orbitalPointInPoint(point: DVec) = {
    val dir = point - f
    f + dir.n * distanceByDir(dir)
  }

  def _travelTimeOnOrbitMsecCCW(orbital_point1: DVec, orbital_point2: DVec): Long = {
    val t1 = tetaDeg360InPoint(orbital_point1)
    val t2 = tetaDeg360InPoint(orbital_point2)
    val wll_pass_r_p = teta_deg_max < t1 && t1 < 360 && 0 < t2 && t2 < teta_deg_min

    val r1 = (orbital_point1 - f).norma
    val r2 = (orbital_point2 - f).norma
    val s = orbital_point1.dist(orbital_point2)

    val chl1 = 1 + (r1 + r2 + s) / (2 * a)
    val chl2 = 1 + (r1 + r2 - s) / (2 * a)

    val l1 = math.log(chl1 + math.sqrt(chl1 * chl1 - 1))
    val l2 = math.log(chl2 + math.sqrt(chl2 * chl2 - 1)) * (if (wll_pass_r_p) -1 else 1)
    ((inv_n * ((math.sinh(l1) - l1) - (math.sinh(l2) - l2))) * 1000).toLong
  }

  def travelTimeOnOrbitMsecCCW(point1: DVec, point2: DVec, recalculate_points: Boolean = false): Long = {
    if (recalculate_points) {
      val orbital_point1 = orbitalPointInPoint(point1)
      val orbital_point2 = orbitalPointInPoint(point2)
      _travelTimeOnOrbitMsecCCW(orbital_point1, orbital_point2)
    } else {
      _travelTimeOnOrbitMsecCCW(point1, point2)
    }
  }

  def travelTimeOnOrbitMsecCCW(teta1Deg360: Double, teta2Deg360: Double): Long = {
    travelTimeOnOrbitMsecCCW(orbitalPointByTrueAnomalyDeg(teta1Deg360), orbitalPointByTrueAnomalyDeg(teta2Deg360))
  }

  def travelTimeOnOrbitMsecCW(point1: DVec, point2: DVec, recalculate_points: Boolean = false): Long = {
    travelTimeOnOrbitMsecCCW(point2, point1, recalculate_points)
  }

  def travelTimeOnOrbitMsecCW(teta1Deg360: Double, teta2Deg360: Double): Long = {
    travelTimeOnOrbitMsecCCW(orbitalPointByTrueAnomalyDeg(teta2Deg360), orbitalPointByTrueAnomalyDeg(teta1Deg360))
  }

  def travelTimeOnOrbitMsec(point1: DVec, point2: DVec, ccw: Boolean, recalculate_points: Boolean = false): Long = {
    if (ccw) travelTimeOnOrbitMsecCCW(point1, point2, recalculate_points)
    else travelTimeOnOrbitMsecCW(point1, point2, recalculate_points)
  }

  def travelTimeOnOrbitMsec(teta1Deg360: Double, teta2Deg360: Double, ccw: Boolean): Long = {
    if (ccw) travelTimeOnOrbitMsecCCW(teta1Deg360, teta2Deg360)
    else travelTimeOnOrbitMsecCW(teta1Deg360, teta2Deg360)
  }

  /**
    * Орбитальная скорость в точке орбиты с данной истинной аномалией. Это скорость относительно притягивающего центра.
    * То есть, если планета в свою очередь движется по орбите, то чтобы получить абсолютную скорость, надо вычислять и прибавлять
    * эту скорость планеты в данный момент времени и так далее.
    *
    * @param teta_rad - угол в радианах между радиус вектором на точку на орбите и направлением на перицентр.
    *                 Если против часовой стрелки - положительный, от 0 до pi, иначе отрицательный, от 0 до -pi
    *                 Рой А. Движение по орбитам. стр. 109
    *                 http://stu.alnam.ru/book_mor-60
    * @return
    */
  def orbitalVelocityValueByTrueAnomalyRad(teta_rad: Double): Double = {
    val orbital_point = orbitalPointByTrueAnomalyRad(teta_rad)
    val r = orbital_point.dist(f)
    math.sqrt(mu * (2 / r + 1 / a))
  }

  def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, ccw: Boolean): DVec = {
    orbitalVelocityInPoint(orbitalPointByTrueAnomalyRad(teta_rad), ccw)
  }

  def orbitalVelocityByDir(dir: DVec): Double = {
    orbitalVelocityValueByTrueAnomalyRad(tetaRad2PiByDir(dir))
  }

  def orbitalVelocityInPoint(point: DVec, ccw: Boolean): DVec = {
    val v = orbitalVelocityValueByTrueAnomalyRad(tetaRad2PiInPoint(point))
    val r = point.dist(f)
    val phi = 3 * math.Pi / 2 + math.sqrt(a * a * (e * e - 1) / (r * (2 * a + r))).myacos // угол между вектором скорости и радиус-вектором
    if (ccw) (point - f).rotateRad(-phi).n * v else (point - f).rotateRad(phi).n * v
  }

  // Балк М.Б. Элементы динамики космического полета. Гл. III, параграф 3 "Решение уравнения Кеплера", стр. 111
  // http://pskgu.ru/ebooks/astro3/astro3_03_03.pdf
  // https://ru.wikipedia.org/wiki/Уравнение_Кеплера
  def orbitalPointAfterTimeCCW(point1: DVec, time_msec: Long): DVec = {
    def _arsh(z: Double) = math.log(z + math.sqrt(z * z + 1))
    def solver(prev_H: Double, M: Double, i: Int = 0, max_i: Int = 100): (Double, Int) = {
      val diff = (e * math.sinh(prev_H) - prev_H - M).abs
      if (diff < 1E-15 || i >= max_i) (prev_H, i)
      else solver(_arsh((prev_H + M) / e), M, i + 1, max_i)
    }
    val t1 = tetaDeg360InPoint(point1)
    val away_from_rp = teta_deg_min >= t1 && t1 >= 0
    val time_from_r_p_to_cur_point_msec = if (away_from_rp) {
      travelTimeOnOrbitMsecCCW(0, t1)
    } else {
      travelTimeOnOrbitMsecCCW(t1, 360)
    }
    val time_from_r_p_msec = if (away_from_rp) {
      time_from_r_p_to_cur_point_msec + time_msec
    } else {
      (time_msec - time_from_r_p_to_cur_point_msec).abs
    }
    val M = 1 / inv_n * (0.001 * time_from_r_p_msec)
    val (resH, iterations) = solver(_arsh((M + M) / e), M)
    if (OrbitalKiller.tacts % 63 == 0) LogUtils.log(s"hyperbolic orbitalPointAfterTimeCCW away $away_from_rp after_r_p ${away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec} i $iterations")
    val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(resH / 2)
    val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
    val teta_res_deg = if (away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec) {
      teta_res_rad / math.Pi * 180
    } else {
      360 - teta_res_rad / math.Pi * 180
    }
    orbitalPointByTrueAnomalyDeg(teta_res_deg)
  }

  def orbitalPointAfterTimeCW(point1: DVec, time_msec: Long): DVec = {
    def _arsh(z: Double) = math.log(z + math.sqrt(z * z + 1))
    def solver(prev_H: Double, M: Double, i: Int = 0, max_i: Int = 100): (Double, Int) = {
      val diff = (e * math.sinh(prev_H) - prev_H - M).abs
      if (diff < 1E-15 || i >= max_i) (prev_H, i)
      else solver(_arsh((prev_H + M) / e), M, i + 1, max_i)
    }
    val t1 = tetaDeg360InPoint(point1)
    val away_from_rp = 360 >= t1 && t1 >= teta_deg_max
    val time_from_r_p_to_cur_point_msec = if (away_from_rp) {
      travelTimeOnOrbitMsecCW(360, t1)
    } else {
      travelTimeOnOrbitMsecCW(t1, 0)
    }
    val time_from_r_p_msec = if (away_from_rp) {
      time_from_r_p_to_cur_point_msec + time_msec
    } else {
      (time_msec - time_from_r_p_to_cur_point_msec).abs
    }
    val M = 1 / inv_n * (0.001 * time_from_r_p_msec)
    val (resH, iterations) = solver(_arsh((M + M) / e), M)
    if (OrbitalKiller.tacts % 63 == 0) LogUtils.log(s"hyperbolic orbitalPointAfterTimeCW away $away_from_rp after_r_p ${away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec} i $iterations")
    val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(resH / 2)
    val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
    val teta_res_deg = if (away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec) {
      360 - teta_res_rad / math.Pi * 180
    } else {
      teta_res_rad / math.Pi * 180
    }
    orbitalPointByTrueAnomalyDeg(teta_res_deg)
  }

  def orbitalPointAfterTime(point1: DVec, time_msec: Long, ccw: Boolean): DVec = {
    if (ccw) orbitalPointAfterTimeCCW(point1, time_msec)
    else orbitalPointAfterTimeCW(point1, time_msec)
  }
}
