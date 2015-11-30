package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._

object TimeToFallTest extends ScageScreenAppD("Time to Fall Test", 640, 480) {
  private val scale = 70e-6

  val earth_radius = 6400000

  val moon_radius = 1737000

  val o = Orbit2(
    a = 1748613.803096609,
    b = 966402.7377262659,
    t = 6560.585270803881,
    //center = DVec(-2.698307864705353E8, 2.701904654918784E8)
    center = DVec(-1457297.4922421277, 0)
  )

  val teta1 = math.acos((o.p/moon_radius - 1)/o.e)/math.Pi*180
  val teta2 = -math.acos((o.p/moon_radius - 1)/o.e)/math.Pi*180 + 360

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  center = DVec.zero

  render {
    drawCircle(DVec.zero, moon_radius*scale, WHITE)

    openglLocalTransform {
      openglMove(o.center * scale)
      openglRotateDeg(Vec(-1, 0).signedDeg(o.f2 - o.f1))
      drawEllipse(DVec.zero, o.a * scale, o.b * scale, WHITE)
    }

    drawFilledCircle(DVec(1,0).rotateDeg(teta1)*o.distanceByTrueAnomalyDeg(teta1)*scale, 3, GREEN)
    drawFilledCircle(DVec(1,0).rotateDeg(teta2)*o.distanceByTrueAnomalyDeg(teta2)*scale, 3, GREEN)

    val mouse_teta = o.tetaDeg360ByDir(absCoord(mouseCoord))
    drawLine(DVec.zero, absCoord(mouseCoord), WHITE)
    drawFilledCircle(DVec(1,0).rotateDeg(mouse_teta)*o.distanceByTrueAnomalyDeg(mouse_teta)*scale, 3, WHITE)
  }

  interface {
    val mouse_teta = o.tetaDeg360ByDir(absCoord(mouseCoord))
    print(f"$mouse_teta%.2f", 20, 20, WHITE)
  }
}
