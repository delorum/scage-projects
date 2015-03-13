package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._

case class Orbit2(
                   a:Double,                     // большая полуось
                   b:Double,                     // малая полуось
                   t:Double,                     // орбитальный период, в секундах
                   center:DVec,                  // координаты центра орбиты-эллипса
                   alpha:Double) {               // угол в градусах, ориентация эллипса (угол большой полуоси с горизонтальной линией)
val e = math.sqrt(math.abs(1 - (b*b)/(a*a)))     // эксцентриситет, характеристика, показывающая степень отклонения от окружности (0 - окружность, <1 - эллипс, 1 - парабола, >1 - гипербола)
val c = a*e                                      // фокальное расстояние (полурасстояние между фокусами)
val p = a*(1 - e*e)                              // фокальный параметр (половина длины хорды, проходящей через фокус и перпендикулярной к фокальной оси)
val r_p = a*(1 - e)                              // перигей
val r_a = a*(1 + e)                              // апогей
val f1 = center + DVec(1,0).rotateDeg(alpha)*c   // координаты первого фокуса
val f2 = center - DVec(1,0).rotateDeg(alpha)*c   // координаты второго фокуса
}

object OrbitPositionTest extends ScageScreenAppD("Orbit Position Test", 640, 480) {
  private var _m:Vec = Vec.zero
  private var _a = 45

  def o = Orbit2(100, 60, 1000, windowCenter, _a)

  def ro = {
    o.p/(1 - o.e*math.cos((_m - o.f1).signedRad(o.f2-o.f1)))
  }

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  mouseMotion(onMotion = m => {
    _m = m
    println(ro)
  })

  action(100) {
    _a = (_a + 1) % 360
  }

  render {
    openglLocalTransform {
      openglMove(o.center)
      openglRotateDeg(Vec(-1,0).signedDeg(o.f2-o.f1))
      drawEllipse(DVec.zero, o.a, o.b, WHITE)
    }
    drawLine(o.f1, _m, WHITE)
    drawFilledCircle(o.f1 + (_m - o.f1).n*ro, 3, WHITE)
  }


}
