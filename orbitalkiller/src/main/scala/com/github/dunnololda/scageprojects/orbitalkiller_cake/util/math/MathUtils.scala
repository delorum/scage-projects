package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math

import scala.annotation.tailrec

object MathUtils {
  @tailrec
  def correctAngle(angle: Double): Double = {
    if (angle > 360) correctAngle(angle - 360)
    else if (angle < 0) correctAngle(angle + 360)
    else angle
  }
}
