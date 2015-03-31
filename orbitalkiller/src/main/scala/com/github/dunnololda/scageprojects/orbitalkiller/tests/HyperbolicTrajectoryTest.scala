package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

object HyperbolicTrajectoryTest extends ScageScreenApp("HyperbolicTrajectoryTest", 800, 600) {
  val e = math.sqrt(2)
  val a = 100
  val f = windowCenter

  val y = (-math.acos(-1.0/e)+0.1 to math.acos(-1.0/e)-0.1 by 0.1).map(ang => {
    val r = a*(e*e - 1)/(1 + e*math.cos(ang))
    f + Vec(1,0).rotateRad(ang)*r
  })
  println(y.head)
  println(y.last)

  var x = -math.acos(-1.0/e)
  action(500) {
    x += 0.1
    if(x >= math.acos(-1.0/e)) x = -math.acos(-1.0/e)
  }

  render {
    drawLine(f, Vec(windowWidth, f.y), WHITE)
    drawSlidingLines(y, WHITE)
    val r = a*(e*e - 1)/(1 + e*math.cos(x))
    drawLine(f, f + Vec(1,0).rotateRad(x)*r, RED)
  }
}
