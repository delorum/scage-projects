package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scage.ScageScreenApp

object ScaledCoordTest extends ScageScreenApp("ScaledCoord Test", 800, 600) {
  private var mouse_coord:Vec = Vec.zero

  private var _center = Vec.zero
  private var _rotation_center = Vec.zero
  private var _rotation_angle_deg = 0f
  private var mode = 1

  key(KEY_W, 100, onKeyDown = if(mode == 1) _center += Vec(0, 5) else _rotation_center += Vec(0,5))
  key(KEY_A, 100, onKeyDown = if(mode == 1) _center -= Vec(5, 0) else _rotation_center -= Vec(5, 0))
  key(KEY_S, 100, onKeyDown = if(mode == 1) _center -= Vec(0, 5) else _rotation_center -= Vec(0, 5))
  key(KEY_D, 100, onKeyDown = if(mode == 1) _center += Vec(5, 0) else _rotation_center += Vec(5, 0))

  key(KEY_1, onKeyDown = mode = 1)
  key(KEY_2, onKeyDown = mode = 2)

  key(KEY_LEFT, 100, onKeyDown = _rotation_angle_deg -= 1)
  key(KEY_RIGHT, 100, onKeyDown = _rotation_angle_deg += 1)

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouse(onBtnDown = m => {
    mouse_coord = absCoord(m)
  })

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 1) globalScale -= 1
    else if(globalScale > 0.1f) globalScale -= 0.1f
  })

  center = _center
  rotationCenter = _rotation_center
  rotationAngleDeg = _rotation_angle_deg
  render {
    (-1005 to 1005 by 15).foreach(x => drawLine(Vec(x, -1005), Vec(x, 1005), DARK_GRAY))
    (-1005 to 1005 by 15).foreach(y => drawLine(Vec(-1005, y), Vec(1005, y), DARK_GRAY))
    drawFilledCircle(mouse_coord, 3, RED)
    /*drawFilledCircle(absCoord(mouse_coord), 3, GREEN)*/
    drawFilledCircle(_rotation_center, 3, YELLOW)
    drawFilledCircle(_center, 3, BLUE)
  }

  interface {
    currentColor = WHITE
    print(s"mode: $mode", 20, 60)
    print(s"coord: $mouse_coord", 20, 40)
    /*print(s"scaled coord: ${absCoord(mouse_coord)}", 20, 20)*/
  }

  /*private def nearestPoint(v:Vec):Vec = {
    Vec(v.ix/15*15-5, v.iy/15*15-5)
  }*/
}
