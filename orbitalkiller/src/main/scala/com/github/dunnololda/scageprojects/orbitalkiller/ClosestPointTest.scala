package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

object ClosestPointTest extends ScageScreenApp("Closest Point", 640 ,480) {
  val l = LineShape(Vec(math.random*640, math.random*480), Vec(math.random*640, math.random*480))

  private var p:Option[Vec] = None

  leftMouse(onBtnDown = m => p = Some(m))

  render {
    drawLine(l.from, l.to, WHITE)
    p.foreach(x => drawLine(x, l.closestPoint(x), YELLOW))
  }
}
