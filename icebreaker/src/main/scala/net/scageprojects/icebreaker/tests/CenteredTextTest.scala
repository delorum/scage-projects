package net.scageprojects.icebreaker.tests

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.Vec

object CenteredTextTest extends ScageScreenApp("Centered Text", 640, 480) {
  private var ang = 0.0f
  val message = "Hello\nWorld\n!"
  action(100) {
    ang += 5f
  }

  render {
    openglMove(windowCenter)
    openglRotate(ang)
    printCentered(message, 0, 0, WHITE)
    drawFilledCircle(Vec.zero, 5, RED)
  }
}
