package net.scage.blases.tests

import net.scage.ScageScreenApp
import net.scage.ScageLib._
import net.scage.support.Vec

object ChangeResolutionTest extends ScageScreenApp("Resolution Change Test", 800, 600, "Resolution Change Test") {
  render {
    print("Press C to change resolution", windowCenter + Vec(0, 40), WHITE)
  }

  private var ang = 0f
  action(100) {
    ang += 5
  }

  render {
    openglMove(windowCenter)
    openglRotate(ang)
    drawFilledPolygon(Array(Vec(-20, -5), Vec(20, -5), Vec(0, 20)), GREEN)
  }

  key(KEY_C, onKeyDown = {
    if (windowWidth == 800 && windowHeight == 600) {
      windowSize = (640, 480)
    } else {
      windowSize = (800, 600)
    }
  })
}
