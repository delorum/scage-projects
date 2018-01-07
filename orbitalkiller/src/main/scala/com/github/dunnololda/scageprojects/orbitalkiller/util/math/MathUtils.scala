package com.github.dunnololda.scageprojects.orbitalkiller.util.math

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.BodyState

/**
  * Created by andrey on 1/6/18.
  */
object MathUtils {
  def correctAngle(angle: Double): Double = {
    if (angle > 360) correctAngle(angle - 360)
    else if (angle < 0) correctAngle(angle + 360)
    else angle
  }

  def curvatureRadiusInPoint(body_state: BodyState): Double = {
    math.abs(body_state.vel.norma2 / (body_state.acc * body_state.vel.p))
  }

  implicit class MyVec(v1: DVec) {
    def deg360(v2: DVec): Double = {
      val scalar = v1.perpendicular * v2
      if (scalar >= 0) v1.deg(v2) else 360 - v1.deg(v2)
    }

    def rad2Pi(v2: DVec): Double = {
      val scalar = v1.perpendicular * v2
      if (scalar >= 0) v1.rad(v2) else 2 * math.Pi - v1.rad(v2)
    }
  }

  implicit class MyDouble(d:Double) {
    def equalPlusMinusOne(x:Double):Boolean = math.abs(d - x) < 1
    def equalPlusMinusTen(x:Double):Boolean = math.abs(d - x) < 10
    def round2Digits:Double = {
      val bd = new java.math.BigDecimal(d)
      bd.setScale(2, java.math.RoundingMode.HALF_UP).doubleValue()
    }

    def myacos:Double = math.acos(math.max(-1, math.min(1, d)))
  }
}
