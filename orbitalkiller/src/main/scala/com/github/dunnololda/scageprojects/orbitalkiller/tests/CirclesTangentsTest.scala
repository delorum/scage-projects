package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

object CirclesTangentsTest extends ScageScreenApp("Circles Tangents Test", 600, 600) {
  val r1 = 50
  val p1 = Vec(100,100)

  val r2 = 10
  val p2 = Vec(500, 500)

  val r3 = r2 - r1
  
  val p2p1 = p2 - p1
  val p2p1_p = p2p1.p

  val b1 = p2 + p2p1_p*r2
  val b2 = p2 - p2p1_p*r2

  val c1 = p1 + p2p1_p*r1
  val c2 = p1 - p2p1_p*r1

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  render {
    drawCircle(p1, r1, WHITE)
    drawCircle(p2, r2, WHITE)

    drawCircle(p2, r3, GREEN)
    drawLine(p1, p2 + p2p1_p*r3, GREEN)
    drawLine(p1, p2 - p2p1_p*r3, GREEN)

    drawLine(c1, b1, WHITE)
    drawLine(c2, b2, WHITE)
  }
}
