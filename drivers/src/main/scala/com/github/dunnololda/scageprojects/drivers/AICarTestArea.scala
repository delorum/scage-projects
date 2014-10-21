package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

object AICarTestArea extends ScageScreenApp("AI Car Test Area", 800, 600) {
  val ai_car = new AICar(Vec(400, 300), 0, this)

  var pos = Vec.zero

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouse(onBtnDown = m => {
    pos = scaledCoord(m)
    ai_car.setWayPoint(pos)
  })

  globalScale = 3

  render {
    drawCircle(pos, 4, WHITE)
  }
}
