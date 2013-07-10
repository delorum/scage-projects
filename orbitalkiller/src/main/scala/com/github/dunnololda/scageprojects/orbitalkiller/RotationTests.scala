package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

object RotationTests extends ScageScreenApp("Rotation Test", 640 ,480) {
  val c = Vec(320, 240)
  render {
    openglLocalTransform {
      openglMove(c)
      openglRotateDeg(45)
      openglMove(Vec(40, 40))
      drawRectCentered(Vec.zero, 20, 20)
      openglMove(Vec(0, -80))
      drawRectCentered(Vec.zero, 20, 20)
      openglMove(Vec(-80, 0))
      drawRectCentered(Vec.zero, 20, 20)
      openglMove(Vec(0, 80))
      drawRectCentered(Vec.zero, 20, 20)
    }
  }
}

