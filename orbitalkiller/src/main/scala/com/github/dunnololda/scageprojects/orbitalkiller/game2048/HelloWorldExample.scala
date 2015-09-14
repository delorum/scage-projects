package com.github.dunnololda.scageprojects.orbitalkiller.game2048

import com.github.dunnololda.scage.ScageLib._

object HelloWorldExample extends ScageScreenApp("Hello World", 640, 480)  {
  private var ang = 0f
  actionStaticPeriod(100) {
    ang += 5
  }

  backgroundColor = BLACK
  render {
    openglMove(windowSize/2)
    openglRotate(ang)
    print("Hello World!", Vec(-50, -5), GREEN)
  }
}
