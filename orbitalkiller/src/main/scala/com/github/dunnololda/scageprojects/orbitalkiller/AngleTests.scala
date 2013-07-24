package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

object AngleTests extends ScageScreenApp("Angle Tests", 640, 480) {
  private var rotation_angle = 0f

  action(100) {
    rotation_angle = (rotation_angle + 1) % 360
  }

  val v1 = Vec(0, 100)

  center = Vec.zero
  render {
    val v2 = v1.rotateDeg(rotation_angle)
    drawLine(Vec.zero, v1, WHITE)
    drawLine(Vec.zero, v2, WHITE)
    val angle = if(v2.x > 0) 360-v1.deg(v2) else v1.deg(v2)
    print(angle, 20, 20, WHITE)
  }
}
