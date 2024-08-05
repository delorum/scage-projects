package com.github.dunnololda.scageprojects.orbitalkiller_cake.util

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.Vector2f

object Phys2dUtils {
  implicit class DVec2DoublePhys2dVector(v: DVec) {
    def toPhys2dVecD = new Vector2f(v.x, v.y)
  }
}
