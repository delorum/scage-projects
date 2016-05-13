package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

object HyperbolicTrajectoryTest extends ScageScreenApp("HyperbolicTrajectoryTest", 800, 600) {
  val e = 1.00001
  val a = 100
  val f = windowCenter

  val y = (-math.acos(-1.0 / e) + 0.1 to math.acos(-1.0 / e) - 0.1 by 0.1).map(ang => {
    val r = a * (e * e - 1) / (1 + e * math.cos(ang))
    f + Vec(1, 0).rotateRad(ang) * r
  })
  println(y.head)
  println(y.last)

  var true_anomaly = -math.acos(-1.0 / e)
  action(500) {
    true_anomaly += 0.1
    if (true_anomaly >= math.acos(-1.0 / e)) true_anomaly = -math.acos(-1.0 / e)
  }

  render {
    drawLine(f, Vec(windowWidth, f.y), WHITE)
    drawSlidingLines(y, WHITE)
    val r = a * (e * e - 1) / (1 + e * math.cos(true_anomaly))
    drawLine(f, f + Vec(1, 0).rotateRad(true_anomaly) * r, RED)

    val test_angle = math.acos((a * (e * e - 1) - r) / (r * e))
    print(f"${true_anomaly.toDeg}%.2f : ${test_angle.toDeg}%.2f", Vec(20, 20), WHITE)
  }
}
