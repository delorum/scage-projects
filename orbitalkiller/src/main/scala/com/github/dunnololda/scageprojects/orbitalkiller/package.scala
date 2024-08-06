package com.github.dunnololda.scageprojects

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main.interfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Collider.findCollisions
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Space
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.Space.splitSpace
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.{BodyState, MutableBodyState}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.MathUtils.correctAngle
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math.factors.Factors.factors5
import com.github.dunnololda.scageprojects.orbitalkiller_cake.{AdditionalSymbols, Main}

import scala.annotation.tailrec
import scala.language.reflectiveCalls

package object orbitalkiller {




  def gravityForce(body1_coord: DVec, body1_mass: Double, body2_coord: DVec, body2_mass: Double, G: Double): DVec = {
    (body1_coord - body2_coord).n * G * body1_mass * body2_mass / body1_coord.dist2(body2_coord)
  }

  private def _satelliteSpeed(from_planet_to_body: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, counterclockwise: Boolean): DVec = {
    val sat_speed = from_planet_to_body.p * math.sqrt(G * planet_mass / from_planet_to_body.norma)
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
    val esc_speed = from_planet_to_body.p * math.sqrt(G * planet_mass / from_planet_to_body.norma) * math.sqrt(2)
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
   * @param perigee_coord - текущая координата корабля, и это будет координата перигея или апогея
   * @param apogee_diff - возвышение противоположной точки: насколько апогей выше перигея (если мы в перигее), либо наоборот, насколько перигей ниже,
   *                    положительное или отрицательное число тут
   * @param planet_coord - координата планеты
   * @param planet_velocity - скорость планеты
   * @param planet_mass - масса планеты
   * @param G - гравитационная постоянная
   * @return двумерный вектор скорости
   */
  def speedToHaveOrbitWithParams(perigee_coord: DVec, apogee_diff: Double, planet_coord: DVec, planet_velocity: DVec, planet_mass: Double, G: Double, ccw: Boolean = true): DVec = {
    val r_p = perigee_coord.dist(planet_coord)
    val r_a = r_p + apogee_diff
    val mu = planet_mass * G
    if(ccw) {
      planet_velocity + math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    } else {
      planet_velocity - math.sqrt(-2 * mu / (r_p + r_a) + 2 * mu / r_p) * (perigee_coord - planet_coord).p
    }
  }

  sealed trait KeplerOrbit {
    def a: Double

    def b: Double

    def e: Double

    def f: DVec

    def center: DVec

    def r_p:Double

    protected def calcFallPos(fall_point:DVec, planet_ang:Double, planet_radius:Double):String = {
      if (interfaceHolder.degOrKm.selectedVariant == 0) {
        f"${correctAngle(DVec(0, 1).deg360(fall_point - f) - planet_ang)}%.3f град."
      } else {
        val planet_length = 2*math.Pi*planet_radius
        val km = (correctAngle(DVec(0, 1).deg360(fall_point - f) - planet_ang) / 360.0 * planet_length) / 1000
        f"$km%.2f/${planet_length/1000}%.2f км"
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

    def orbitalPointByTrueAnomalyDeg(angle_deg:Double):DVec
    def tetaRad2PiInPoint(p:DVec):Double
    def distanceByTrueAnomalyRad(angle_rad:Double):Double
    def orbitalVelocityValueByTrueAnomalyRad(teta_rad: Double):Double
    def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, ccw:Boolean):DVec
    def orbitalPointAfterTime(point1: DVec, time_msec: Long, ccw: Boolean): DVec
    def withNewFocusPosition(new_f:DVec):KeplerOrbit
    def orbitalPointInPoint(p:DVec):DVec
    def travelTimeOnOrbitMsec(from:DVec, to:DVec, ccw:Boolean, recalculate_orbital_points: Boolean = false):Long
    def orbitalPointAfterTimeCCW(coord:DVec, flight_time_msec:Long):DVec
    def orbitalVelocityInPoint(point: DVec, ccw:Boolean):DVec
  }

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
                      mu: Double) extends KeplerOrbit {
    // гравитационный параметр: произведение гравитационной постоянной G на сумму масс притягивающего центра и корабля на орбите
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
      val dir = if (ccw) {
        AdditionalSymbols.ccw_symbol // против часовой стрелки
        } else {
        AdditionalSymbols.cw_symbol // по часовой стрелке
        }
      if (r_p - planet_radius < 0) {
        val y_axis = (ship_coord - f).n
        //val y0 = (ship_coord - f)*y_axis - planet_radius
        val v0y = (ship_velocity - planet_velocity) * y_axis
        //val fall_time_sec = (v0y + math.sqrt(2*planet_g*y0 + v0y*v0y))/planet_g
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
          (0l, fall_pos)
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

        val allowed_acc = if (interfaceHolder.gSwitcher.maxGSet) interfaceHolder.gSwitcher.maxG * planet_g else 1000000 / Main.player_ship.mass
        val time_to_stop_at_full_power = math.abs(v0y / (allowed_acc - planet_g))
        val fall_time_str = if (fall_time_msec < 500) "" else if (fall_time_msec < 30000) s"[r Поверхность через ${timeStrSec(fall_time_msec)}, $fall_position (${timeStrMsec((time_to_stop_at_full_power * 1000l).toLong)})]" else s"Поверхность через ${timeStrMsec(fall_time_msec)}, $fall_position"

        f"$prefix, суборбитальная, $dir, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, r_a = ${mOrKmOrMKm(r_a - planet_radius)}. $fall_time_str"
      } else {
        f"$prefix, замкнутая, $dir, e = $e%.2f, r_p = ${mOrKmOrMKm(r_p - planet_radius)}, r_a = ${mOrKmOrMKm(r_a - planet_radius)}, t = ${timeStrSec((t * 1000l).toLong)}"
      }
    }

    lazy val f_minus_f2_n = Option((f - f2).n).filterNot(_.isZero).getOrElse(DVec(0,1))
    val inv_n = a * math.sqrt(a / mu) // это 1/n

    def tetaDeg360ByDir(dir: DVec) = f_minus_f2_n.deg360(dir)

    def tetaSignedDegByDir(dir: DVec) = f_minus_f2_n.signedDeg(dir)

    def tetaRad2PiByDir(dir: DVec) = f_minus_f2_n.rad2Pi(dir)

    def tetaSignedRadByDir(dir: DVec) = f_minus_f2_n.signedRad(dir)

    def tetaDeg360InPoint(p: DVec) = tetaDeg360ByDir(p - f)

    def tetaSignedDegInPoint(p: DVec) = tetaSignedDegByDir(p - f)

    def tetaRad2PiInPoint(p: DVec) = tetaRad2PiByDir(p - f)

    def tetaSignedRadInPoint(p: DVec) = tetaSignedRadByDir(p - f)

    def tetaRadByDistance(r: Double): Double = {
      ((p / r - 1) / e).myacos
    }

    def tetaDegByDistance(r: Double): Double = {
      tetaRadByDistance(r) / math.Pi * 180.0
    }

    def distanceByTrueAnomalyRad(teta_rad: Double) = {
      p / (1 + e * math.cos(teta_rad))
    }

    def distanceByTrueAnomalyDeg(teta_deg: Double) = {
      p / (1 + e * math.cos(teta_deg / 180.0 * math.Pi))
    }

    /**
     * Расстояние от притягивающего центра до орбиты в данном направлении. Определяем истинную аномалию данного направления
     * и по формуле считаем длину радиус-вектора.
     * https://en.wikipedia.org/wiki/True_anomaly#Radius_from_true_anomaly
     * @param dir - вектор направления
     * @return
     */
    def distanceByDir(dir: DVec) = {
      p / (1 + e * math.cos(tetaSignedRadByDir(dir)))
    }

    def distanceInPoint(point: DVec) = {
      p / (1 + e * math.cos(tetaSignedRadInPoint(point)))
    }

    def orbitalPointByTrueAnomalyRad(teta_rad: Double) = {
      f + f_minus_f2_n.rotateRad(teta_rad) * distanceByTrueAnomalyRad(teta_rad)
    }

    def orbitalPointByTrueAnomalyDeg(teta_deg: Double) = {
      f + f_minus_f2_n.rotateDeg(teta_deg) * distanceByTrueAnomalyDeg(teta_deg)
    }

    def orbitalPointByDir(dir: DVec) = {
      f + dir.n * distanceByDir(dir)
    }

    def orbitalPointInPoint(point: DVec) = {
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
      } else if(t2 == 0) {
        if(t1 > 180) (xl1, xl2, "None")
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
    def travelTimeOnOrbitMsecCCW(point1: DVec, point2: DVec, recalculate_orbital_points: Boolean = false /*, print_variant:Boolean = false*/): Long = {
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

    def travelTimeOnOrbitMsecCCW(teta1Deg360: Double, teta2Deg360: Double /*, print_variant:Boolean*/): Long = {
      val orbital_point1 = orbitalPointByTrueAnomalyDeg(teta1Deg360)
      val orbital_point2 = orbitalPointByTrueAnomalyDeg(teta2Deg360)
      _travelTimeOnOrbitMsecCCW(teta1Deg360, orbital_point1, teta2Deg360, orbital_point2)
    }

    def travelTimeOnOrbitMsecCW(point1: DVec, point2: DVec, recalculate_orbital_points: Boolean = false): Long = {
      travelTimeOnOrbitMsecCCW(point2, point1, recalculate_orbital_points)
    }

    def travelTimeOnOrbitMsecCW(teta1Deg360: Double, teta2Deg360: Double /*, print_variant:Boolean*/): Long = {
      val orbital_point1 = orbitalPointByTrueAnomalyDeg(teta1Deg360)
      val orbital_point2 = orbitalPointByTrueAnomalyDeg(teta2Deg360)
      _travelTimeOnOrbitMsecCCW(teta2Deg360, orbital_point2, teta1Deg360, orbital_point1)
    }

    def travelTimeOnOrbitMsec(point1: DVec, point2: DVec, ccw: Boolean, recalculate_orbital_points: Boolean = false): Long = {
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
    def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, ccw:Boolean):DVec = {
      val vr = math.sqrt(mu / p) * e * math.sin(teta_rad)
      val vt = math.sqrt(mu / p) * (1 + e * math.cos(teta_rad))
      val r = f_minus_f2_n.rotateRad(teta_rad)
      val t = r.perpendicular
      if(ccw) vr*r + vt*t  else -vr*r - vt*t
    }

    def orbitalVelocityByDir(dir: DVec, ccw:Boolean):DVec = {
      orbitalVelocityByTrueAnomalyRad(tetaRad2PiByDir(dir), ccw)
    }

    def orbitalVelocityInPoint(point: DVec, ccw:Boolean):DVec = {
      orbitalVelocityByTrueAnomalyRad(tetaRad2PiInPoint(point), ccw)
    }

    def orbitalVelocityValueByTrueAnomalyRad(teta_rad: Double):Double = {
      orbitalVelocityByTrueAnomalyRad(teta_rad, ccw = true).norma
    }

    // Балк М.Б. Элементы динамики космического полета. Гл. III, параграф 3 "Решение уравнения Кеплера", стр. 111
    // http://pskgu.ru/ebooks/astro3/astro3_03_03.pdf
    // https://en.wikipedia.org/wiki/Kepler%27s_equation
    def orbitalPointAfterTimeCCW(point1: DVec, time_msec: Long): DVec = {
      val t1 = tetaDeg360InPoint(point1)
      val time_from_r_p_msec = travelTimeOnOrbitMsecCCW(0, t1 /*, print_variant = true*/)
      val all_time_msec = time_from_r_p_msec + time_msec
      val M = 1 / inv_n * (0.001*all_time_msec)
      val E1 = M + e * math.sin(M)
      def solver(prev_E:Double = E1, i:Int = 0, max_i:Int = 100):(Double, Int) = {
        val diff = (prev_E - e*math.sin(prev_E) - M).abs
        if(diff < 1E-15 || i >= max_i) (prev_E, i)
        else solver(M + e*math.sin(prev_E), i+1, max_i)
      }
      val (res_E, i) = solver()
      if(Main.system_evolution.tacts % 63 == 0) println(s"elliptic orbitalPointAfterTimeCCW i $i")
      val tg_half_teta_res_rad = math.sqrt((1 + e) / (1 - e)) * math.tan(res_E / 2)
      val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
      val teta_res_deg = teta_res_rad / math.Pi * 180
      orbitalPointByTrueAnomalyDeg(teta_res_deg)
    }

    def orbitalPointAfterTimeCW(point1: DVec, time_msec: Long): DVec = {
      val t1 = tetaDeg360InPoint(point1)
      val time_from_r_p_msec = travelTimeOnOrbitMsecCW(0, t1 /*, print_variant = true*/)
      val all_time_msec = t*1000 - (time_from_r_p_msec + time_msec)
      val M = 1 / inv_n * (0.001*all_time_msec)
      val E1 = M + e * math.sin(M)
      def solver(prev_E:Double = E1, i:Int = 0, max_i:Int = 100):(Double, Int) = {
        val diff = (prev_E - e*math.sin(prev_E) - M).abs
        if(diff < 1E-15 || i >= max_i) (prev_E, i)
        else solver(M + e*math.sin(prev_E), i+1, max_i)
      }
      val (resE, iterations) = solver()
      if(Main.system_evolution.tacts % 63 == 0) println(s"elliptic orbitalPointAfterTimeCW i $iterations")
      val tg_half_teta_res_rad = math.sqrt((1 + e) / (1 - e)) * math.tan(resE / 2)
      val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
      val teta_res_deg = teta_res_rad / math.Pi * 180
      orbitalPointByTrueAnomalyDeg(teta_res_deg)
    }

    def orbitalPointAfterTime(point1: DVec, time_msec: Long, ccw: Boolean): DVec = {
      if (ccw) orbitalPointAfterTimeCCW(point1, time_msec)
      else orbitalPointAfterTimeCW(point1, time_msec)
    }

    def withNewFocusPosition(new_f:DVec):EllipseOrbit = {
      val new_f2 = new_f - (f - f2)
      val new_center = (new_f2 - new_f).n * c + new_f
      new EllipseOrbit(a, b, e, c, p, r_p, r_a, t, new_f, new_f2, new_center, mu)
    }

    def centerIfFocusPosition(new_f:DVec):DVec = {
      (f2 - f).n * c + new_f
    }
  }

  class HyperbolaOrbit(
                        val a: Double, // большая полуось
                        val b: Double,
                        val e: Double, // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
                        val f: DVec, // координаты первого фокуса (координаты небесного тела, вокруг которого вращаемся)
                        val center: DVec, // координаты центра
                        val mu: Double) extends KeplerOrbit {
    def withNewFocusPosition(new_f:DVec):HyperbolaOrbit = {
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
      val dir = if (ccw) AdditionalSymbols.ccw_symbol else AdditionalSymbols.cw_symbol
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

        val allowed_acc = if (interfaceHolder.gSwitcher.maxGSet) interfaceHolder.gSwitcher.maxG * planet_g else 1000000 / Main.player_ship.mass
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
      val l2 = math.log(chl2 + math.sqrt(chl2 * chl2 - 1))*(if(wll_pass_r_p) -1 else 1)
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
     * @param teta_rad - угол в радианах между радиус вектором на точку на орбите и направлением на перицентр.
     *                 Если против часовой стрелки - положительный, от 0 до pi, иначе отрицательный, от 0 до -pi
     *                 Рой А. Движение по орбитам. стр. 109
     *                 http://stu.alnam.ru/book_mor-60
     * @return
     */
    def orbitalVelocityValueByTrueAnomalyRad(teta_rad: Double):Double = {
      val orbital_point = orbitalPointByTrueAnomalyRad(teta_rad)
      val r = orbital_point.dist(f)
      math.sqrt(mu * (2 / r + 1 / a))
    }

    def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, ccw:Boolean):DVec = {
      orbitalVelocityInPoint(orbitalPointByTrueAnomalyRad(teta_rad), ccw)
    }

    def orbitalVelocityByDir(dir: DVec):Double = {
      orbitalVelocityValueByTrueAnomalyRad(tetaRad2PiByDir(dir))
    }

    def orbitalVelocityInPoint(point: DVec, ccw:Boolean):DVec = {
      val v = orbitalVelocityValueByTrueAnomalyRad(tetaRad2PiInPoint(point))
      val r = point.dist(f)
      val phi = 3*math.Pi/2 + math.sqrt(a * a * (e * e - 1) / (r * (2 * a + r))).myacos // угол между вектором скорости и радиус-вектором
      if(ccw) (point - f).rotateRad(-phi).n*v else (point - f).rotateRad(phi).n*v
    }

    // Балк М.Б. Элементы динамики космического полета. Гл. III, параграф 3 "Решение уравнения Кеплера", стр. 111
    // http://pskgu.ru/ebooks/astro3/astro3_03_03.pdf
    // https://ru.wikipedia.org/wiki/Уравнение_Кеплера
    def orbitalPointAfterTimeCCW(point1: DVec, time_msec: Long): DVec = {
      def _arsh(z: Double) = math.log(z + math.sqrt(z * z + 1))
      def solver(prev_H:Double, M:Double, i:Int = 0, max_i:Int = 100):(Double, Int) = {
        val diff = (e*math.sinh(prev_H) - prev_H - M).abs
        if(diff < 1E-15 || i >= max_i) (prev_H, i)
        else solver(_arsh((prev_H + M) / e), M, i+1, max_i)
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
      val M = 1 / inv_n * (0.001*time_from_r_p_msec)
      val (resH, iterations) = solver(_arsh((M + M) / e), M)
      if(Main.system_evolution.tacts % 63 == 0) println(s"hyperbolic orbitalPointAfterTimeCCW away $away_from_rp after_r_p ${away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec} i $iterations")
      val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(resH / 2)
      val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
      val teta_res_deg = if(away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec) {
        teta_res_rad / math.Pi * 180
      } else {
        360 - teta_res_rad / math.Pi * 180
      }
      orbitalPointByTrueAnomalyDeg(teta_res_deg)
    }

    def orbitalPointAfterTimeCW(point1: DVec, time_msec: Long): DVec = {
      def _arsh(z: Double) = math.log(z + math.sqrt(z * z + 1))
      def solver(prev_H:Double, M:Double, i:Int = 0, max_i:Int = 100):(Double, Int) = {
        val diff = (e*math.sinh(prev_H) - prev_H - M).abs
        if(diff < 1E-15 || i >= max_i) (prev_H, i)
        else solver(_arsh((prev_H + M) / e), M, i+1, max_i)
      }
      val t1 = tetaDeg360InPoint(point1)
      val away_from_rp = 360 >= t1 && t1 >= teta_deg_max
      val time_from_r_p_to_cur_point_msec = if(away_from_rp) {
        travelTimeOnOrbitMsecCW(360, t1)
      } else {
        travelTimeOnOrbitMsecCW(t1, 0)
      }
      val time_from_r_p_msec = if(away_from_rp) {
        time_from_r_p_to_cur_point_msec + time_msec
      } else {
       (time_msec - time_from_r_p_to_cur_point_msec).abs
      }
      val M = 1 / inv_n * (0.001*time_from_r_p_msec)
      val (resH, iterations) = solver(_arsh((M + M) / e), M)
      if(Main.system_evolution.tacts % 63 == 0) println(s"hyperbolic orbitalPointAfterTimeCW away $away_from_rp after_r_p ${away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec} i $iterations")
      val tg_half_teta_res_rad = math.sqrt((e + 1) / (e - 1)) * math.tanh(resH / 2)
      val teta_res_rad = math.atan(tg_half_teta_res_rad) * 2
      val teta_res_deg = if(away_from_rp || time_msec >= time_from_r_p_to_cur_point_msec) {
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

  /**
   * Вычисляет параметры орбиты
   * @param planet_mass - масса планеты
   * @param planet_coord - абсолютная координата планеты
   * @param body_mass - масса тела, вращающегося вокруг планеты
   * @param body_relative_coord - относительная координата тела (абсолютная координата тела минус абсолютная координата планеты)
   * @param body_relative_velocity - относительная линейная скорость тела (абсолютная скорость тела минус абсолютная скорость планеты)
   * @param G - гравитационная постоянная
   * @return объект Orbit, содержащий вычисленный набор параметров
   */
  def calculateOrbit(planet_mass: Double, planet_coord: DVec, body_mass: Double, body_relative_coord: DVec, body_relative_velocity: DVec, G: Double): KeplerOrbit = {
    //https://ru.wikipedia.org/wiki/Гравитационный_параметр
    //val mu = (planet_mass + body_mass) * G // гравитационный параметр
    val mu = planet_mass*G // гравитационный параметр

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

  def specificOrbitalEnergy(planet_mass: Double, planet_coord: DVec, body_mass: Double, body_relative_coord: DVec, body_relative_velocity: DVec, G: Double): Double = {
    val mu = (planet_mass + body_mass) * G // гравитационный параметр
    body_relative_velocity.norma2 / 2 - mu / body_relative_coord.norma
  }

  /**
   *
   * @param force - вектор силы
   * @param force_position_from_mass_center - точка приложения силы относительно центра масс
   * @param sin_angle - синус угла между вектором от центра масс до точки приложения силы и вектором силы
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

  def maxOption[T](l: Seq[T])(implicit o: Ordering[T]): Option[T] = if (l.isEmpty) None else Some(l.max(o))

  implicit class MyVec(v1: DVec) {
    def deg360(v2: DVec): Double = {
      val scalar = v1.perpendicular * v2
      if (scalar >= 0) v1.deg(v2) else 360 - v1.deg(v2)
    }

    def rad2Pi(v2: DVec): Double = {
      val scalar = v1.perpendicular * v2
      if (scalar >= 0) v1.rad(v2) else 2 * math.Pi - v1.rad(v2)
    }
  }

  def checkAllConditions(conditions: (() => Boolean)*): Boolean = {
    if (conditions.isEmpty) true
    else if (conditions.head()) checkAllConditions(conditions.tail: _*)
    else false
  }

  implicit class MyDouble(d:Double) {
    def equalPlusMinusOne(x:Double):Boolean = math.abs(d - x) < 1
    def equalPlusMinusTen(x:Double):Boolean = math.abs(d - x) < 10
    def round2Digits:Double = {
      val bd = new java.math.BigDecimal(d)
      bd.setScale(2, java.math.RoundingMode.HALF_UP).doubleValue()
    }

    def myacos:Double = math.acos(math.max(-1, math.min(1, d)))
  }
}
