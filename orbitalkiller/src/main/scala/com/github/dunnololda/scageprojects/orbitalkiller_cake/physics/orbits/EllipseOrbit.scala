package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.orbits

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.{AdditionalSymbols, Main}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main.interfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils.{mOrKmOrMKm, timeStrMsec, timeStrSec}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.{MyDouble, MyVec}

import scala.annotation.tailrec

class EllipseOrbit(
    val a: Double, // большая полуось
    val b: Double, // малая полуось
    val e: Double, // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
    val c: Double, // фокальное расстояния (полурасстояние между фокусами)
    val p: Double, // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)
    val r_p: Double, // перигей
    val r_a: Double, // апогей
    val t: Double, // орбитальный период, в секундах
    val f: DVec, // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
    val f2: DVec, // координаты второго фокуса
    val center: DVec, // координаты центра
    mu: Double)
  extends KeplerOrbit {

  // гравитационный параметр: произведение гравитационной постоянной G на сумму масс притягивающего центра и корабля на орбите
  def strDefinition(
      prefix: String,
      planet_radius: Double,
      planet_velocity: DVec,
      planet_ang: Double,
      planet_groundSpeedMsec: Double,
      planet_g: Double,
      ship_coord: DVec,
      ship_velocity: DVec,
      ship_radius: Double): String = {
    val ccw = (ship_coord - f).perpendicular * (ship_velocity - planet_velocity) >= 0 // летим против часовой?
    val dir = if (ccw) {
      AdditionalSymbols.ccw_symbol // против часовой стрелки
    } else {
      AdditionalSymbols.cw_symbol // по часовой стрелке
    }
    if (r_p - planet_radius < 0) {
      val y_axis = (ship_coord - f).n
      // val y0 = (ship_coord - f)*y_axis - planet_radius
      val v0y = (ship_velocity - planet_velocity) * y_axis
      // val fall_time_sec = (v0y + math.sqrt(2*planet_g*y0 + v0y*v0y))/planet_g
      /*val fall_time_sec = {
        // https://www.rand.org/content/dam/rand/pubs/research_memoranda/2008/RM3752.pdf
        // page 6-7
        val r_L = ship_coord.dist(f)
        val r_T = planet_radius + 3.5
        val V_sL = math.sqrt(planet_g*planet_radius*planet_radius/r_L)
        val V_L = (ship_velocity - planet_velocity).norma
        val Y_L = V_L/V_sL
        val gamma_L = (ship_velocity - planet_velocity).rad(y_axis.perpendicular)
        val t_LA = r_L/(V_sL*math.pow(2 - Y_L*Y_L, 3.0/2.0))*(math.acos((1 - Y_L*Y_L)/e) + Y_L*math.sin(gamma_L*math.sqrt(2 - Y_L*Y_L)))
        val t_AT = r_L/(V_sL*math.pow(2 - Y_L*Y_L, 3.0/2.0))*(math.acos(((1 - Y_L*Y_L) + (1 - r_L/r_T))/(r_L/r_T*e)) + math.sqrt(2 - Y_L*Y_L)/(r_L/r_T)*math.sqrt(2*(r_L/r_T - 1) + Y_L*Y_L*(1 - math.pow(r_L/r_T, 2)*math.pow(math.cos(gamma_L), 2))))
        val launch_before_apogee = v0y >= 0
        if(launch_before_apogee) t_LA + t_AT else -t_LA + t_AT
      }*/

      /*val ship_planet_vertical_speed = (ship_velocity - planet_velocity) * (ship_coord - f).n
      val ship_planet_tangent_speed = ((ship_velocity - planet_velocity) * (ship_coord - f).p) / ship_coord.dist(f) * planet_radius - planet_groundSpeedMsec*/
      val (fall_time_msec, fall_position) = if ((ship_coord.dist(f) - planet_radius) < ship_radius /* &&
                                ship_planet_vertical_speed.abs < 0.5 &&
                                ship_planet_tangent_speed.abs < 0.5*/ ) {
        val fall_pos = calcFallPos(ship_coord, planet_ang, planet_radius)
        (0L, fall_pos)
      } else if (ccw) {
        val fall_teta_rad = -((p / (planet_radius + 3) - 1) / e).myacos + 2 * math.Pi
        val fall_point = orbitalPointByTrueAnomalyRad(fall_teta_rad)
        val fall_pos = calcFallPos(fall_point, planet_ang, planet_radius)
        val fall_time = travelTimeOnOrbitMsecCCW(ship_coord, fall_point)
        (fall_time, fall_pos)
      } else {
        val fall_teta_rad = ((p / (planet_radius + 3) - 1) / e).myacos
        val fall_point = orbitalPointByTrueAnomalyRad(fall_teta_rad)
        val fall_pos = calcFallPos(fall_point, planet_ang, planet_radius)
        val fall_time = travelTimeOnOrbitMsecCW(ship_coord, fall_point)
        (fall_time, fall_pos)
      }

      val allowed_acc =
        if (interfaceHolder.gSwitcher.maxGSet) interfaceHolder.gSwitcher.maxG * planet_g
        else 1000000 / Main.player_ship.mass
      val time_to_stop_at_full_power = math.abs(v0y / (allowed_acc - planet_g))
      val fall_time_str =
        if (fall_time_msec < 500) ""
        else if (fall_time_msec < 30000)
          s"[r Поверхность через ${timeStrSec(fall_time_msec)}, $fall_position (${timeStrMsec((time_to_stop_at_full_power * 1000L).toLong)})]"
        else s"Поверхность через ${timeStrMsec(fall_time_msec)}, $fall_position"

      f"$prefix, суборбитальная, $dir, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, r_a = ${mOrKmOrMKm(r_a - planet_radius)}. $fall_time_str"
    } else {
      f"$prefix, замкнутая, $dir, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, r_a = ${mOrKmOrMKm(
          r_a - planet_radius
        )}, t = ${timeStrSec((t * 1000L).toLong)}"
    }
  }

  lazy val f_minus_f2_n: DVec = Option((f - f2).n).filterNot(_.isZero).getOrElse(DVec(0, 1))
  val inv_n: Double = a * math.sqrt(a / mu) // это 1/n

  def tetaDeg360ByDir(dir: DVec): Double = f_minus_f2_n.deg360(dir)

  def tetaSignedDegByDir(dir: DVec): Double = f_minus_f2_n.signedDeg(dir)

  def tetaRad2PiByDir(dir: DVec): Double = f_minus_f2_n.rad2Pi(dir)

  def tetaSignedRadByDir(dir: DVec): Double = f_minus_f2_n.signedRad(dir)

  def tetaDeg360InPoint(p: DVec): Double = tetaDeg360ByDir(p - f)

  def tetaSignedDegInPoint(p: DVec): Double = tetaSignedDegByDir(p - f)

  def tetaRad2PiInPoint(p: DVec): Double = tetaRad2PiByDir(p - f)

  def tetaSignedRadInPoint(p: DVec): Double = tetaSignedRadByDir(p - f)

  def tetaRadByDistance(r: Double): Double = {
    ((p / r - 1) / e).myacos
  }

  def tetaDegByDistance(r: Double): Double = {
    tetaRadByDistance(r) / math.Pi * 180.0
  }

  def distanceByTrueAnomalyRad(teta_rad: Double): Double = {
    p / (1 + e * math.cos(teta_rad))
  }

  def distanceByTrueAnomalyDeg(teta_deg: Double): Double = {
    p / (1 + e * math.cos(teta_deg / 180.0 * math.Pi))
  }

  /**
   * Расстояние от притягивающего центра до орбиты в данном направлении. Определяем истинную аномалию данного направления
   * и по формуле считаем длину радиус-вектора.
   * https://en.wikipedia.org/wiki/True_anomaly#Radius_from_true_anomaly
   * @param dir - вектор направления
   * @return
   */
  def distanceByDir(dir: DVec): Double = {
    p / (1 + e * math.cos(tetaSignedRadByDir(dir)))
  }

  def distanceInPoint(point: DVec): Double = {
    p / (1 + e * math.cos(tetaSignedRadInPoint(point)))
  }

  def orbitalPointByTrueAnomalyRad(teta_rad: Double): DVec = {
    f + f_minus_f2_n.rotateRad(teta_rad) * distanceByTrueAnomalyRad(teta_rad)
  }

  def orbitalPointByTrueAnomalyDeg(teta_deg: Double): DVec = {
    f + f_minus_f2_n.rotateDeg(teta_deg) * distanceByTrueAnomalyDeg(teta_deg)
  }

  def orbitalPointByDir(dir: DVec): DVec = {
    f + dir.n * distanceByDir(dir)
  }

  def orbitalPointInPoint(point: DVec): DVec = {
    val dir = point - f
    f + dir.n * distanceByDir(dir)
  }

  private def _travelTimeOnOrbitMsecCCW(t1: Double, orbital_point1: DVec, t2: Double, orbital_point2: DVec): Long = {
    val r1 = (orbital_point1 - f).norma
    val r2 = (orbital_point2 - f).norma
    val s = orbital_point1.dist(orbital_point2)
    val xl1 = (1 - (r1 + r2 + s) / (2 * a)).myacos
    val xl2 = (1 - (r1 + r2 - s) / (2 * a)).myacos
    // Балк М.Б. Элементы динамики космического полета, Формула Ламберта, стр 128-129: выбор чисел l1, l2 среди корней уравнения
    // для эллиптической орбиты, анализ проведен английским математиком А. Кэли
    val (l1, l2, _) = if (t1 == 0) {
      if (t2 < 180) (xl1, xl2, "None")
      else (2 * math.Pi - xl1, -xl2, "F & A")
    } else if (t2 == 0) {
      if (t1 > 180) (xl1, xl2, "None")
      else (2 * math.Pi - xl1, -xl2, "F & A")
    } else {
      if (areLinesIntersect(f2 - f_minus_f2_n * r_p, f2, orbital_point1, orbital_point2)) {
        if (t2 > t1) (xl1, xl2, "None")
        else (2 * math.Pi - xl1, -xl2, "F & A")
      } else if (areLinesIntersect(f, f + f_minus_f2_n * r_p, orbital_point1, orbital_point2)) {
        if (t1 > t2) (xl1, xl2, "None")
        else (2 * math.Pi - xl1, -xl2, "F & A")
      } else if (areLinesIntersect(f2, f, orbital_point1, orbital_point2)) {
        if (t2 > t1) (2 * math.Pi - xl1, xl2, "F")
        else (xl1, -xl2, "A")
      } else {
        if (t2 > t1) {
          (xl1, xl2, "None")
        } else {
          (2 * math.Pi - xl1, -xl2, "F & A")
        }
      }
    }
    /*if(print_variant) {
      println(variant)
      /*if(variant == "F & A") {
        drawLine(orbital_point1*Main.scale, orbital_point2*Main.scale, YELLOW)
      }*/
    }*/
    // Балк М.Б. Элементы динамики космического полета, Формула Ламберта
    ((inv_n * ((l1 - math.sin(l1)) - (l2 - math.sin(l2)))) * 1000).toLong
  }

  /**
   * Время в миллисекундах, которое займет перемещение корабля по эллиптической орбите из точки point1 в точку point2 против часово стрелки.
   * Вычисляется по формуле Ламберта, которую нашел в книге М.Б. Балка "Элементы динамики космического полета", стр 122-129
   * http://pskgu.ru/ebooks/astro3/astro3_03_05.pdf
   * @param point1 - начальная точка
   * @param point2 - конечная точка
   * @return
   */
  def travelTimeOnOrbitMsecCCW(
      point1: DVec,
      point2: DVec,
      recalculate_orbital_points: Boolean = false /*, print_variant:Boolean = false*/): Long = {
    val t1 = tetaDeg360InPoint(point1)
    val t2 = tetaDeg360InPoint(point2)
    if (recalculate_orbital_points) {
      val orbital_point1 = orbitalPointByTrueAnomalyDeg(t1)
      val orbital_point2 = orbitalPointByTrueAnomalyDeg(t2)
      _travelTimeOnOrbitMsecCCW(t1, orbital_point1, t2, orbital_point2)
    } else {
      _travelTimeOnOrbitMsecCCW(t1, point1, t2, point2)
    }
  }

  def travelTimeOnOrbitMsecCCW(teta1Deg360: Double, teta2Deg360: Double /*, print_variant:Boolean*/ ): Long = {
    val orbital_point1 = orbitalPointByTrueAnomalyDeg(teta1Deg360)
    val orbital_point2 = orbitalPointByTrueAnomalyDeg(teta2Deg360)
    _travelTimeOnOrbitMsecCCW(teta1Deg360, orbital_point1, teta2Deg360, orbital_point2)
  }

  def travelTimeOnOrbitMsecCW(point1: DVec, point2: DVec, recalculate_orbital_points: Boolean = false): Long = {
    travelTimeOnOrbitMsecCCW(point2, point1, recalculate_orbital_points)
  }

  def travelTimeOnOrbitMsecCW(teta1Deg360: Double, teta2Deg360: Double /*, print_variant:Boolean*/ ): Long = {
    val orbital_point1 = orbitalPointByTrueAnomalyDeg(teta1Deg360)
    val orbital_point2 = orbitalPointByTrueAnomalyDeg(teta2Deg360)
    _travelTimeOnOrbitMsecCCW(teta2Deg360, orbital_point2, teta1Deg360, orbital_point1)
  }

  def travelTimeOnOrbitMsec(
      point1: DVec,
      point2: DVec,
      ccw: Boolean,
      recalculate_orbital_points: Boolean = false): Long = {
    if (ccw) travelTimeOnOrbitMsecCCW(point1, point2, recalculate_orbital_points)
    else travelTimeOnOrbitMsecCW(point1, point2, recalculate_orbital_points)
  }

  def travelTimeOnOrbitMsec(teta1Deg360: Double, teta2Deg360: Double, ccw: Boolean): Long = {
    if (ccw) travelTimeOnOrbitMsecCCW(teta1Deg360, teta2Deg360)
    else travelTimeOnOrbitMsecCW(teta1Deg360, teta2Deg360)
  }

  /**
   * Орбитальная скорость в точке орбиты с данной истинной аномалией. Это скорость относительно притягивающего центра.
   * То есть, если планета в свою очередь движется по орбите, то чтобы получить абсолютную скорость, надо вычислять и прибавлять
   * эту скорость планеты в данный момент времени и так далее.
   * @param teta_rad - угол в радианах между радиус вектором на точку на орбите и направлением на перицентр.
   *                 Если против часовой стрелки - положительный, от 0 до pi, иначе отрицательный, от 0 до -pi
   *                 https://en.wikipedia.org/wiki/Lambert%27s_problem
   *                 https://en.wikipedia.org/wiki/Kepler_orbit
   * @return vr - радиальная компонента, направлена к притягивающему центру. r = (position_on_orbit - planet.coord).n
   *         vt - перпендикулярна радиальной компоненте. t = r.perpendicular
   *         math.sqrt(vr*vr + vt*vt) даст числовое выражение скорости
   */
  def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, ccw: Boolean): DVec = {
    val vr = math.sqrt(mu / p) * e * math.sin(teta_rad)
    val vt = math.sqrt(mu / p) * (1 + e * math.cos(teta_rad))
    val r = f_minus_f2_n.rotateRad(teta_rad)
    val t = r.perpendicular
    if (ccw) vr * r + vt * t else -vr * r - vt * t
  }

  def orbitalVelocityByDir(dir: DVec, ccw: Boolean): DVec = {
    orbitalVelocityByTrueAnomalyRad(tetaRad2PiByDir(dir), ccw)
  }

  def orbitalVelocityInPoint(point: DVec, ccw: Boolean): DVec = {
    orbitalVelocityByTrueAnomalyRad(tetaRad2PiInPoint(point), ccw)
  }

  def orbitalVelocityValueByTrueAnomalyRad(teta_rad: Double): Double = {
    orbitalVelocityByTrueAnomalyRad(teta_rad, ccw = true).norma
  }

  // Балк М.Б. Элементы динамики космического полета. Гл. III, параграф 3 "Решение уравнения Кеплера", стр. 111
  // http://pskgu.ru/ebooks/astro3/astro3_03_03.pdf
  // https://en.wikipedia.org/wiki/Kepler%27s_equation
  def orbitalPointAfterTimeCCW(point1: DVec, time_msec: Long): DVec = {
    val t1 = tetaDeg360InPoint(point1)
    val time_from_r_p_msec = travelTimeOnOrbitMsecCCW(0, t1 /*, print_variant = true*/ )
    val all_time_msec = time_from_r_p_msec + time_msec
    val M = 1 / inv_n * (0.001 * all_time_msec)
    val E1 = M + e * math.sin(M)
    @tailrec
    def solver(prev_E: Double = E1, i: Int = 0, max_i: Int = 100): (Double, Int) = {
      val diff = (prev_E - e * math.sin(prev_E) - M).abs
      if (diff < 1e-15 || i >= max_i) (prev_E, i)
      else solver(M + e * math.sin(prev_E), i + 1, max_i)
    }
    val (res_E, i) = solver()
    if (Main.system_evolution.tacts % 63 == 0) println(s"elliptic orbitalPointAfterTimeCCW i $i")
    val tg_half_teta_res_rad = math.sqrt((1 + e) / (1 - e)) * math.tan(res_E / 2)
    val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
    val teta_res_deg = teta_res_rad / math.Pi * 180
    orbitalPointByTrueAnomalyDeg(teta_res_deg)
  }

  def orbitalPointAfterTimeCW(point1: DVec, time_msec: Long): DVec = {
    val t1 = tetaDeg360InPoint(point1)
    val time_from_r_p_msec = travelTimeOnOrbitMsecCW(0, t1 /*, print_variant = true*/ )
    val all_time_msec = t * 1000 - (time_from_r_p_msec + time_msec)
    val M = 1 / inv_n * (0.001 * all_time_msec)
    val E1 = M + e * math.sin(M)
    @tailrec
    def solver(prev_E: Double = E1, i: Int = 0, max_i: Int = 100): (Double, Int) = {
      val diff = (prev_E - e * math.sin(prev_E) - M).abs
      if (diff < 1e-15 || i >= max_i) (prev_E, i)
      else solver(M + e * math.sin(prev_E), i + 1, max_i)
    }
    val (resE, iterations) = solver()
    if (Main.system_evolution.tacts % 63 == 0) println(s"elliptic orbitalPointAfterTimeCW i $iterations")
    val tg_half_teta_res_rad = math.sqrt((1 + e) / (1 - e)) * math.tan(resE / 2)
    val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
    val teta_res_deg = teta_res_rad / math.Pi * 180
    orbitalPointByTrueAnomalyDeg(teta_res_deg)
  }

  def orbitalPointAfterTime(point1: DVec, time_msec: Long, ccw: Boolean): DVec = {
    if (ccw) orbitalPointAfterTimeCCW(point1, time_msec)
    else orbitalPointAfterTimeCW(point1, time_msec)
  }

  def withNewFocusPosition(new_f: DVec): EllipseOrbit = {
    val new_f2 = new_f - (f - f2)
    val new_center = (new_f2 - new_f).n * c + new_f
    new EllipseOrbit(a, b, e, c, p, r_p, r_a, t, new_f, new_f2, new_center, mu)
  }

  def centerIfFocusPosition(new_f: DVec): DVec = {
    (f2 - f).n * c + new_f
  }
}
