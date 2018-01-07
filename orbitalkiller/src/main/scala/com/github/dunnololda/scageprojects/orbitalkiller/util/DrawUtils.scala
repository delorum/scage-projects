package com.github.dunnololda.scageprojects.orbitalkiller.util

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.util.math.GeometryUtils._
import com.github.dunnololda.scageprojects.orbitalkiller.components.BasicComponents._

/**
  * Created by andrey on 1/7/18.
  */
object DrawUtils {
  def drawArrow(from1: DVec, to1: DVec, color: ScageColor, scale: Double = globalScale) {
    val arrow11 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(15)
    val arrow12 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(-15)
    drawLine(from1, to1, color)
    drawLine(to1, arrow11, color)
    drawLine(to1, arrow12, color)
  }

  def drawDashedLine(from: DVec, to: DVec, dash_len: Double, color: ScageColor): Unit = {
    val line_len = (to - from).norma
    val normal = (to - from).n
    (0.0 to line_len - dash_len by dash_len * 2).foreach { dash_from =>
      drawLine(from + normal * dash_from, from + normal * (dash_from + dash_len), color)
    }
  }

  def drawDashedArrow(from1: DVec,
                      to1: DVec,
                      dash_len: Double,
                      color: ScageColor,
                      scale: Double = globalScale): Unit = {
    val line_len = (to1 - from1).norma
    val normal = (to1 - from1).n
    (0.0 to line_len - dash_len by dash_len * 2).foreach { dash_from =>
      drawLine(from1 + normal * dash_from, from1 + normal * (dash_from + dash_len), color)
    }
    val arrow11 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(15)
    val arrow12 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(-15)
    drawLine(to1, arrow11, color)
    drawLine(to1, arrow12, color)
  }

  def drawSunTangents(planet_coord: DVec, planet_radius: Double, sun_coord: DVec, sun_radius: Double, dist: Double) {
    tangentsFromCircleToCircle(planet_coord, planet_radius, sun_coord, sun_radius) match {
      case Some((c1, c2, b1, b2)) =>
        val a = (c1 - b1).n * dist
        val b = (c2 - b2).n * dist
        drawLine(c1 * scale, (c1 + a) * scale, DARK_GRAY)
        drawLine(c2 * scale, (c2 + b) * scale, DARK_GRAY)
      case None =>
    }
  }
}
