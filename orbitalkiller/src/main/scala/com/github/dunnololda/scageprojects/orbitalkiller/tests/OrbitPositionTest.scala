package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._

case class Orbit2(
                   a: Double, // большая полуось
                   b: Double, // малая полуось
                   t: Double, // орбитальный период, в секундах
                   center: DVec) {
  // координаты центра орбиты-эллипса
  val e = math.sqrt(math.abs(1 - (b * b) / (a * a)))
  // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
  val c = a * e
  // фокальное расстояние (полурасстояние между фокусами)
  val p = a * (1 - e * e)
  // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)
  val r_p = a * (1 - e)
  // перигей
  val r_a = a * (1 + e)
  // апогей
  val f1 = center + DVec(1, 0) * c
  // координаты первого фокуса
  val f2 = center - DVec(1, 0) * c // координаты второго фокуса

  private val f1_minus_f2 = f1 - f2

  def tetaDeg360ByDir(dir: DVec) = f1_minus_f2.deg360(dir)

  def tetaSignedDegByDir(dir: DVec) = f1_minus_f2.signedDeg(dir)

  def tetaRad2PiByDir(dir: DVec) = f1_minus_f2.rad2Pi(dir)

  def tetaSignedRadByDir(dir: DVec) = f1_minus_f2.signedRad(dir)

  def tetaDeg360InPoint(p: DVec) = tetaDeg360ByDir(p - f1)

  def tetaSignedDegInPoint(p: DVec) = tetaSignedDegByDir(p - f1)

  def tetaRad2PiInPoint(p: DVec) = tetaRad2PiByDir(p - f1)

  def tetaSignedRadInPoint(p: DVec) = tetaSignedRadByDir(p - f1)

  def tetaRadByDistance(r: Double): Double = {
    math.acos((p / r - 1) / e)
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
    f1 + f1_minus_f2.rotateRad(teta_rad).n * distanceByTrueAnomalyRad(teta_rad)
  }

  def orbitalPointByTrueAnomalyDeg(teta_deg: Double) = {
    f1 + f1_minus_f2.rotateDeg(teta_deg).n * distanceByTrueAnomalyDeg(teta_deg)
  }

  def orbitalPointByDir(dir: DVec) = {
    f1 + dir.n * distanceByDir(dir)
  }

  def orbitalPointInPoint(point: DVec) = {
    val dir = point - f1
    f1 + dir.n * distanceByDir(dir)
  }

  /**
   * Время в миллисекундах, которое займет перемещение корабля по эллиптической орбите из точки point1 в точку point2
   * против часовой стрелки.
   * Вычисляется по формуле Ламберта, которую нашел в книге М.Б. Балка "Элементы динамики космического полета", стр 122-129
   * http://pskgu.ru/ebooks/astro3/astro3_03_05.pdf
   * @param point1 - начальная точка
   * @param point2 - конечная точка
   * @return
   */
  def travelTimeOnOrbitMsecCCW(point1: DVec, point2: DVec, mu: Double): (Long, String) = {
    val t1 = tetaDeg360InPoint(point1)
    val t2 = tetaDeg360InPoint(point2)
    val orbital_point1 = f1 + (point1 - f1).n * distanceInPoint(point1)
    val orbital_point2 = f1 + (point2 - f1).n * distanceInPoint(point2)
    val r1 = (orbital_point1 - f1).norma
    val r2 = (orbital_point2 - f1).norma
    val s = orbital_point1.dist(orbital_point2)
    val xl1 = math.acos(1 - (r1 + r2 + s) / (2 * a))
    val xl2 = math.acos(1 - (r1 + r2 - s) / (2 * a))
    // Балк М.Б. Элементы динамики космического полета, Формула Ламберта, стр 128-129: выбор чисел l1, l2 среди корней уравнения
    // для эллиптической орбиты, анализ проведен английским математиком А. Кэли
    val (l1, l2, variant) = if (t1 == 0) {
      if (t2 < 180) (xl1, xl2, "None")
      else (2 * math.Pi - xl1, -xl2, "F & A")
    } else if(t2 == 0) {
      if(t1 > 180) (xl1, xl2, "None")
      else (2 * math.Pi - xl1, -xl2, "F & A")
    } else {
      if (areLinesIntersect(f2 + (f2 - f1).n * r_p, f2, orbital_point1, orbital_point2)) {
        if (t2 > t1) (xl1, xl2, "None")
        else (2 * math.Pi - xl1, -xl2, "F & A")
      } else if (areLinesIntersect(f1, f1 + (f1 - f2).n * r_p, orbital_point1, orbital_point2)) {
        if (t1 > t2) (xl1, xl2, "None")
        else (2 * math.Pi - xl1, -xl2, "F & A")
      } else if (areLinesIntersect(f2, f1, orbital_point1, orbital_point2)) {
        if (t2 > t1) (2 * math.Pi - xl1, xl2, "F")
        else (xl1, -xl2, "A")
      } else {
        if (t2 > t1) (xl1, xl2, "None")
        else (2 * math.Pi - xl1, -xl2, "F & A")
      }
    }
    val n_1 = a * math.sqrt(a / mu) // это 1/n
    // Балк М.Б. Элементы динамики космического полета, Формула Ламберта
    ((n_1 * ((l1 - math.sin(l1)) - (l2 - math.sin(l2)))).toLong * 1000, variant)
  }

  /**
   * Орбитальная скорость в точке орбиты с данной истинной аномалией
   * @param teta_rad - угол в радианах между радиус вектором на точку на орбите и направлением на перицентр.
   *                 Если против часовой стрелки - положительный, от 0 до pi, иначе отрицательный, от 0 до -pi
   *                 https://en.wikipedia.org/wiki/Lambert%27s_problem
   *                 https://en.wikipedia.org/wiki/Kepler_orbit
   * @return
   */
  def orbitalVelocityByTrueAnomalyRad(teta_rad: Double, mu: Double) = {
    val vr = math.sqrt(mu / p) * e * math.sin(teta_rad)
    val vt = math.sqrt(mu / p) * (1 + e * math.cos(teta_rad))
    (vt, vr)
  }

  def orbitalVelocityByDir(dir: DVec, mu: Double) = {
    orbitalVelocityByTrueAnomalyRad(tetaRad2PiByDir(dir), mu)
  }

  def orbitalVelocityInPoint(point: DVec, mu: Double) = {
    orbitalVelocityByTrueAnomalyRad(tetaRad2PiInPoint(point), mu)
  }
}

object OrbitPositionTest extends ScageScreenAppD("Orbit Position Test", 640, 640) {
  val o = Orbit2(
    a = 1.0922509227094337E7,
    b = 1.0338913703140972E7,
    t = 11358.125220688678,
    center = DVec(6.595622340768156E-4, 3522509.227094339)
  )

  private var _m: DVec = DVec.zero

  val G: Double = 6.6742867E-11
  val earth_mass = 5.9746E24
  val mu = G * earth_mass

  //private var mr1:Option[DVec] = Some(o.f1 + (o.f1 - o.f2).n.rotateDeg(45)*ro(o.f1 + (o.f1 - o.f2).n.rotateDeg(45)))
  //private var mr1:Option[DVec] = Some(o.f1 + (o.f1 - o.f2).n*o.r_p)
  private var mr1: Option[DVec] = Some(o.f1 + (o.f1 - o.f2).p * o.r_p)
  private var mr2: Option[DVec] = Some(o.f1 + (o.f1 - o.f2).n * o.r_p)
  //private var flight_time:Option[List[String]] = None
  val (time, variant) = o.travelTimeOnOrbitMsecCCW(mr1.get, mr2.get, mu)
  private var flight_time: Option[String] = Some(s"${timeStr(time)}, $variant")

  def msecOrKmsec(msec: Number): String = {
    if (math.abs(msec.doubleValue()) < 1000) f"${msec.doubleValue()}%.2f м/сек" else f"${msec.doubleValue() / 1000}%.2f км/сек"
  }

  def timeStr(time_msec: Long): String = {
    val is_below_zero = time_msec < 0
    val abs_time_msec = math.abs(time_msec)
    val result = if (abs_time_msec < 1000) s"$abs_time_msec мсек."
    else {
      val sec = 1000l
      val min = sec * 60
      val hour = min * 60
      val day = hour * 24

      List(
        (abs_time_msec / day, "д."),
        (abs_time_msec % day / hour, "ч."),
        (abs_time_msec % hour / min, "мин."),
        (abs_time_msec % min / sec, "сек."),
        (abs_time_msec % sec, "мсек.")
      ).filter(_._1 > 0).map(e => e._1 + " " + e._2).mkString(" ")
    }
    if (is_below_zero) s"-$result" else result
  }

  val n_1 = o.a * math.sqrt(o.a / mu)

  def ro(m: DVec) = {
    o.p / (1 - o.e * math.cos((m - o.f1).signedRad(o.f2 - o.f1)))
  }

  keyIgnorePause(KEY_Q, onKeyDown = {
    if (keyPressed(KEY_LCONTROL)) stopApp()
  })

  val polygon_step = 1

  leftMouseIgnorePause(onBtnDown = m => {
    val mm = absCoord(m) / scale
    mr1 = Some(o.f1 + (mm - o.f1).n * ro(mm))
    /*if(mr1.isEmpty) {
      mr1 = Some(o.f1 + (mm - o.f1).n*ro(mm))
    } else if(mr2.isEmpty) {
      mr2 = Some(o.f1 + (mm - o.f1).n * ro(mm))
    }*/
  })

  /*mouseMotion(onMotion = m => {
    _m = absCoord(m)
    val mm = _m / scale
    mr2 = Some(o.f1 + (mm - o.f1).n * ro(mm))
    val (time, variant) = o.travelTimeOnOrbitMsecCCW(mr1.get, mr2.get, mu)
    flight_time = Some(s"${timeStr(time)}, $variant")
    //println(ro(_m/scale))
  })
*/
  /*action(100) {
    _a = (_a + 1) % 360
  }*/

  /*var x = 255
  var y = 255
  var z = 255

  key(KEY_Q, 100, onKeyDown = x += 1)
  key(KEY_A, 100, onKeyDown = x -= 1)
  key(KEY_W, 100, onKeyDown = y += 1)
  key(KEY_S, 100, onKeyDown = y -= 1)
  key(KEY_E, 100, onKeyDown = z += 1)
  key(KEY_D, 100, onKeyDown = z -= 1)*/

  val scale = 10e-6
  center = o.center * scale

  render {
    openglLocalTransform {
      openglMove(o.center * scale)
      openglRotateDeg(Vec(-1, 0).signedDeg(o.f2 - o.f1))
      drawEllipse(DVec.zero, o.a * scale, o.b * scale, WHITE)
    }
    drawLine(o.f1 * scale, _m, WHITE)
    drawLine(o.f1 * scale, o.f1 * scale + (o.f1 - o.f2).n * o.r_p * scale, WHITE)
    drawFilledCircle(o.f1 * scale + (_m / scale - o.f1).n * ro(_m / scale) * scale, 3, WHITE)
    if (mr1.nonEmpty) {
      drawFilledCircle(mr1.get * scale, 3, RED)
      //drawLine(mr1.get*scale, o.f1*scale, YELLOW)
    }
    if (mr2.nonEmpty) {
      drawFilledCircle(mr2.get * scale, 3, YELLOW)
    }
    if (mr1.nonEmpty && mr2.nonEmpty) {
      drawLine(mr1.get * scale, mr2.get * scale, YELLOW)
      val t1 = (o.f1 - o.f2).deg360(mr1.get - o.f1)
      val xt2 = (o.f1 - o.f2).deg360(mr2.get - o.f1)
      val t2 = if (xt2 < t1) xt2 + 360 else xt2
      val l = if ((t2 - t1) % polygon_step == 0) (t1 to t2 by polygon_step).toList else (t1 to t2 by polygon_step).toList ::: List(t2)
      val polygon = l.map(deg => {
        val mm = o.f1 + DVec(1, 0).rotateDeg(deg)
        val v = o.f1 + (mm - o.f1).n * ro(mm)
        v * scale
      })
      drawFilledPolygon(polygon, YELLOW)
    }
    drawFilledCircle(o.f1 * scale, 3, RED)
    drawFilledCircle(o.f2 * scale, 3, RED)
  }

  interface {
    print(f"mr1 angle = ${(o.f1 - o.f2).deg360(mr1.get - o.f1)}%.2f", 20, 20, WHITE)
    if (mr2.nonEmpty) print(f"mr2 angle = ${(o.f1 - o.f2).deg360(mr2.get - o.f1)}%.2f", 20, 40, WHITE)
    if (mr1.nonEmpty && mr2.nonEmpty) {
      val t1 = (o.f1 - o.f2).deg360(mr1.get - o.f1)
      val xt2 = (o.f1 - o.f2).deg360(mr2.get - o.f1)
      val t2 = if (xt2 < t1) xt2 + 360 else xt2
      print(f"diff = ${t2 - t1}%.2f", 20, 60, WHITE)
    }
    if (flight_time.nonEmpty) {
      //print(s"${flight_time.get.mkString(" : ")}", 20, 60, WHITE)
      print(s"${flight_time.get}", 20, 80, WHITE)
    }
  }

  /*interface {
    //print("Test", 20, 40, YELLOW)
    val c = new ScageColor("Orange",x, y, z)
    print(s"Message $x $y $z", 20, 40, c)
    print(s"$c", 20, 20, c)
  }*/
}
