package com.github.dunnololda.scageprojects.orbitalkiller.util.math

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.physics.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller.celestials.CelestialBody

/**
  * Created by andrey on 1/7/18.
  */
object GeometryUtils {
  def circlesIntersection(p0: DVec, r0: Double, p1: DVec, r1: Double): List[DVec] = {
    val d = p1.dist(p0)
    if (d > r0 + r1 || d < math.abs(r0 - r1)) Nil
    else if (d == r0 + r1) {
      List(p0 + (p1 - p0).n * r0)
    } else {
      val a = (r0 * r0 - r1 * r1 + d * d) / (2 * d)
      val h = math.sqrt(r0 * r0 - a * a)
      val DVec(x2, y2) = p0 + a * (p1 - p0) / d
      List(DVec(x2 + h * (p1.y - p0.y) / d, y2 - h * (p1.x - p0.x) / d),
        DVec(x2 - h * (p1.y - p0.y) / d, y2 + h * (p1.x - p0.x) / d))
    }
  }

  def tangentsFromPointToCircle(p: DVec, c: DVec, r: Double): List[DVec] = {
    circlesIntersection(c, r, p + (c - p) * 0.5, c.dist(p) / 2)
  }

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
}
