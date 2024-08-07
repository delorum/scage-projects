package com.github.dunnololda.scageprojects.orbitalkiller_cake.util

import com.github.dunnololda.scage.ScageLibD.{DVec, ScageColor, drawLine}

object DrawUtils {
  def drawArrow(from1: DVec, to1: DVec, color: ScageColor, scale: Double): Unit = {
    val arrow11 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(15)
    val arrow12 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(-15)
    drawLine(from1, to1, color)
    drawLine(to1, arrow11, color)
    drawLine(to1, arrow12, color)
  }

  def drawDashedLine(from: DVec, to: DVec, dash_len: Double, color: ScageColor): Unit = {
    val line_len = (to - from).norma
    val normal = (to - from).n
    (0.0 to line_len - dash_len by dash_len * 2).foreach(dash_from =>
      drawLine(from + normal * dash_from, from + normal * (dash_from + dash_len), color)
    )
  }

  def drawDashedArrow(
                       from1: DVec,
                       to1: DVec,
                       dash_len: Double,
                       color: ScageColor,
                       scale: Double): Unit = {
    val line_len = (to1 - from1).norma
    val normal = (to1 - from1).n
    (0.0 to line_len - dash_len by dash_len * 2).foreach(dash_from =>
      drawLine(from1 + normal * dash_from, from1 + normal * (dash_from + dash_len), color)
    )
    val arrow11 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(15)
    val arrow12 = to1 + ((from1 - to1).n * 10 / scale).rotateDeg(-15)
    drawLine(to1, arrow11, color)
    drawLine(to1, arrow12, color)
  }
}
