package com.github.dunnololda.scageprojects.orbitalkiller.util.math

/**
  * Created by andrey on 1/6/18.
  */
object MathUtils {
  def correctAngle(angle: Double): Double = {
    if (angle > 360) correctAngle(angle - 360)
    else if (angle < 0) correctAngle(angle + 360)
    else angle
  }
}
