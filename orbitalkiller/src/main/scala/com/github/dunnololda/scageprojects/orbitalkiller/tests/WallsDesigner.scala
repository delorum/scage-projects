package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

object WallsDesigner extends ScageScreenApp("Walls Designer", 800, 600) {
  private var _center = Vec.zero
  private val walls = collection.mutable.HashSet[Vec]()

  key(KEY_W, 100, onKeyDown = _center += Vec(0, 5))
  key(KEY_A, 100, onKeyDown = _center -= Vec(5, 0))
  key(KEY_S, 100, onKeyDown = _center -= Vec(0, 5))
  key(KEY_D, 100, onKeyDown = _center += Vec(5, 0))

  key(KEY_C, onKeyDown = {
    if (keyPressed(KEY_LCONTROL)) walls.clear()
  })
  key(KEY_Q, onKeyDown = {
    if (keyPressed(KEY_LCONTROL)) stopApp()
  })

  leftMouse(onBtnDown = m => {
    val v = absCoord(m)
    walls += Vec((v.ix + 1005) / 15 * 15 - 1005 + 7.5f, (v.iy + 1005) / 15 * 15 - 1005 + 7.5f)
  })

  rightMouse(onBtnDown = m => {
    val v = absCoord(m)
    walls -= Vec((v.ix + 1005) / 15 * 15 - 1005 + 7.5f, (v.iy + 1005) / 15 * 15 - 1005 + 7.5f)
  })

  leftMouseDrag(onDrag = m => {
    val v = absCoord(m)
    walls += Vec((v.ix + 1005) / 15 * 15 - 1005 + 7.5f, (v.iy + 1005) / 15 * 15 - 1005 + 7.5f)
  })

  rightMouseDrag(onDrag = m => {
    val v = absCoord(m)
    walls -= Vec((v.ix + 1005) / 15 * 15 - 1005 + 7.5f, (v.iy + 1005) / 15 * 15 - 1005 + 7.5f)
  })

  mouseWheelUp(onWheelUp = m => {
    if (globalScale < 1) globalScale += 0.1f
    else if (globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if (globalScale > 1) globalScale -= 1
    else if (globalScale > 0.1) globalScale -= 0.1f
  })

  center = _center

  render {
    (-1005 to 1005 by 15).foreach(x => drawLine(Vec(x, -1005), Vec(x, 1005), DARK_GRAY))
    (-1005 to 1005 by 15).foreach(y => drawLine(Vec(-1005, y), Vec(1005, y), DARK_GRAY))

    walls.foreach {
      case w => drawFilledRectCentered(w, 15, 15, GRAY)
    }
  }

  interface {
    print(walls.size, 20, 20, WHITE)
  }
}
