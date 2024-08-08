package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.math

import com.github.dunnololda.scage.ScageLibD._

import scala.annotation.tailrec

object MathUtils {
  @tailrec
  def correctAngle(angle: Double): Double = {
    if (angle > 360) correctAngle(angle - 360)
    else if (angle < 0) correctAngle(angle + 360)
    else angle
  }

  def circlesIntersection(p0: DVec, r0: Double, p1: DVec, r1: Double): List[DVec] = {
    val d = p1.dist(p0)
    if (d > r0 + r1 || d < math.abs(r0 - r1)) Nil
    else if (d == r0 + r1) {
      List(p0 + (p1 - p0).n * r0)
    } else {
      val a = (r0 * r0 - r1 * r1 + d * d) / (2 * d)
      val h = math.sqrt(r0 * r0 - a * a)
      val DVec(x2, y2) = p0 + a * (p1 - p0) / d
      List(
        DVec(x2 + h * (p1.y - p0.y) / d, y2 - h * (p1.x - p0.x) / d),
        DVec(x2 - h * (p1.y - p0.y) / d, y2 + h * (p1.x - p0.x) / d)
      )
    }
  }

  def tangentsFromPointToCircle(p: DVec, c: DVec, r: Double): List[DVec] = {
    circlesIntersection(c, r, p + (c - p) * 0.5, c.dist(p) / 2)
  }

  @tailrec
  def tangentsFromCircleToCircle(p0: DVec, r0: Double, p1: DVec, r1: Double): Option[(DVec, DVec, DVec, DVec)] = {
    if (r0 > r1) tangentsFromCircleToCircle(p1, r1, p0, r0)
    else {
      val l = tangentsFromPointToCircle(p0, p1, r1 - r0)
      if (l.length != 2) None
      else {
        val List(x1, x2) = l
        val x1p2_n = (x1 - p1).n
        val x2p2_n = (x2 - p1).n

        val b1 = p1 + x1p2_n * r1
        val b2 = p1 + x2p2_n * r1

        val c1 = p0 + x1p2_n * r0
        val c2 = p0 + x2p2_n * r0
        Some((c1, c2, b1, b2))
      }
    }
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
