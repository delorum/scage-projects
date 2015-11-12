package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._

case class Orbit2(
                   a:Double,                     // большая полуось
                   b:Double,                     // малая полуось
                   t:Double,                     // орбитальный период, в секундах
                   center:DVec) {                 // координаты центра орбиты-эллипса
val e = math.sqrt(math.abs(1 - (b*b)/(a*a)))     // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
val c = a*e                                      // фокальное расстояние (полурасстояние между фокусами)
val p = a*(1 - e*e)                              // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)
val r_p = a*(1 - e)                              // перигей
val r_a = a*(1 + e)                              // апогей
val f1 = center + DVec(1,0)*c   // координаты первого фокуса
val f2 = center - DVec(1,0)*c   // координаты второго фокуса
}

object OrbitPositionTest extends ScageScreenAppD("Orbit Position Test", 640, 640) {
  implicit class MyVec(v1:DVec) {
    def deg360(v2:DVec):Double = {
      val scalar = v2*v1.perpendicular
      if(scalar >= 0) v2.deg(v1) else 360 - v2.deg(v1)
    }
  }

  private var _m:DVec = DVec.zero

  private var mr1:Option[DVec] = None
  private var mr2:Option[DVec] = None
  //private var flight_time:Option[List[String]] = None
  private var flight_time:Option[String] = None

  val o = Orbit2(
    a = 1.0922509227094337E7,
    b = 1.0338913703140972E7,
    t = 11358.125220688678,
    center = DVec(6.595622340768156E-4, 3522509.227094339)
  )

  val G:Double = 6.6742867E-11
  val earth_mass = 5.9746E24
  val mu = G*earth_mass

  def msecOrKmsec(msec:Number):String = {
    if(math.abs(msec.doubleValue()) < 1000) f"${msec.doubleValue()}%.2f м/сек" else f"${msec.doubleValue()/1000}%.2f км/сек"
  }

  def timeStr(time_msec:Long):String = {
    val is_below_zero = time_msec < 0
    val abs_time_msec = math.abs(time_msec)
    val result = if (abs_time_msec < 1000) s"$abs_time_msec мсек."
    else {
      val sec  = 1000l
      val min  = sec*60
      val hour = min*60
      val day  = hour*24

      List(
        (abs_time_msec/day,      "д."),
        (abs_time_msec%day/hour, "ч."),
        (abs_time_msec%hour/min, "мин."),
        (abs_time_msec%min/sec,  "сек."),
        (abs_time_msec%sec,      "мсек.")
      ).filter(_._1 > 0).map(e => e._1+" "+e._2).mkString(" ")
    }
    if(is_below_zero) s"-$result" else result
  }

  val n_1 = o.a*math.sqrt(o.a/mu)

  def ro(m:DVec) = {
    o.p/(1 - o.e*math.cos((m - o.f1).signedRad(o.f2-o.f1)))
  }

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouseIgnorePause(onBtnDown = m => {
    val mm = absCoord(m)/scale
    if(mr1.isEmpty) {
      mr1 = Some(o.f1 + (mm - o.f1).n*ro(mm))
    } else if(mr2.isEmpty) {
      mr2 = Some(o.f1 + (mm - o.f1).n*ro(mm))
      val r1 = (mr1.get - o.f1).norma
      val t1 = (o.f1-o.f2).deg360(mr1.get - o.f1)
      //println(t1)
      val r2 = (mr2.get - o.f1).norma
      val t2 = (o.f1-o.f2).deg360(mr2.get - o.f1)
      //println(t2)
      //println(t2 - t1)
      val s = mr1.get.dist(mr2.get)
      val xl1 = math.acos(1 - (r1+r2+s)/(2*o.a))
      val xl2 = math.acos(1 - (r1+r2-s)/(2*o.a))

      def _detectCase(t1:Double, xt2:Double, l1:Double, l2:Double):(Double, Double, String) = {
        val t2 = if(xt2 < t1) xt2 + 360 else xt2
        val polygon = (t1 to t2 by 1).map(deg => {
          val mm = o.f1 + DVec(1, 0).rotateDeg(deg)
          val v = o.f1 + (mm - o.f1).n * ro(mm)
          v
        }).toList
        if (coordOnArea(o.f1, polygon)) {
          if (coordOnArea(o.f2, polygon)) {
            (2 * math.Pi - l1, -l2, "F & A")
          } else {
            (l1, -l2, "A")
          }
        } else {
          if (coordOnArea(o.f2, polygon)) {
            (2 * math.Pi - l1, l2, "F")
          } else {
            (l1, l2, "None")
          }
        }
      }
      val (l1, l2, variant) = _detectCase(t1, t2, xl1, xl2)
      /*def _printTimes:List[String] = {
        List((xl1, xl2), (2*math.Pi - xl1, -xl2), (xl1, -xl2), (2*math.Pi-xl1, xl2)).map {
          case (x1, x2) =>
            timeStr((n_1*((x1 - math.sin(x1)) - (x2 - math.sin(x2)))).toLong*1000)
        }
      }*/
      flight_time = Some(s"${timeStr((n_1*((l1 - math.sin(l1)) - (l2 - math.sin(l2)))).toLong*1000)}, $variant")
      //flight_time = Some(_printTimes ::: List(variant))
    } else {
      mr1 = None
      mr2 = None
      flight_time = None
    }
  })

  mouseMotion(onMotion = m => {
    _m = absCoord(m)
    //println(ro(_m/scale))
  })

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
  center = o.center*scale

  render {
    openglLocalTransform {
      openglMove(o.center*scale)
      openglRotateDeg(Vec(-1,0).signedDeg(o.f2-o.f1))
      drawEllipse(DVec.zero, o.a*scale, o.b*scale, WHITE)
    }
    drawLine(o.f1*scale, _m, WHITE)
    drawLine(o.f1*scale, o.f1*scale + (o.f1 - o.f2).n*o.r_p*scale, WHITE)
    drawFilledCircle(o.f1*scale + (_m/scale - o.f1).n*ro(_m/scale)*scale, 3, WHITE)
    if(mr1.nonEmpty) {
      drawFilledCircle(mr1.get*scale, 3, RED)
      //drawLine(mr1.get*scale, o.f1*scale, YELLOW)
    }
    if(mr2.nonEmpty) {
      drawFilledCircle(mr2.get*scale, 3, YELLOW)
    }
    if(mr1.nonEmpty && mr2.nonEmpty) {
      drawLine(mr1.get*scale, mr2.get*scale, YELLOW)
      val t1 = (o.f1-o.f2).deg360(mr1.get - o.f1)
      val xt2 = (o.f1-o.f2).deg360(mr2.get - o.f1)
      val t2 = if(xt2 < t1) xt2 + 360 else xt2
      drawFilledPolygon((t1 to t2 by 1).map(deg => {
        val mm = o.f1 + DVec(1,0).rotateDeg(deg)
        val v = o.f1 + (mm - o.f1).n*ro(mm)
        v*scale
      }).toList, YELLOW)
    }
    drawFilledCircle(o.f1*scale, 3, RED)
    drawFilledCircle(o.f2*scale, 3, RED)
  }

  interface {
    print(f"teta = ${(o.f1-o.f2).deg360(_m/scale - o.f1)}%.2f", 20, 20, WHITE)
    val teta = (_m/scale - o.f1).rad(o.f1-o.f2)
    val vr = math.sqrt(mu/o.p)*o.e*math.sin(teta)
    val vt = math.sqrt(mu/o.p)*(1 + o.e*math.cos(teta))
    val v = math.sqrt(vr*vr + vt*vt)
    print(s"${msecOrKmsec(v)}", 20, 40, WHITE)
    if(flight_time.nonEmpty) {
      //print(s"${flight_time.get.mkString(" : ")}", 20, 60, WHITE)
      print(s"${flight_time.get}", 20, 60, WHITE)
    }
  }

  /*interface {
    //print("Test", 20, 40, YELLOW)
    val c = new ScageColor("Orange",x, y, z)
    print(s"Message $x $y $z", 20, 40, c)
    print(s"$c", 20, 20, c)
  }*/
}
