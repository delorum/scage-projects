package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._

object ScaledCoordTest extends ScageScreenAppD("ScaledCoord Test", 800, 600) {
  private var mouse_coord: DVec = DVec.zero

  private val _base = DVec(-270836988, 270836988)
  //private val _base = DVec.zero
  private var _center = _base
  private var _rotation_center = _center
  private var _rotation_angle_deg = 0.0
  private var mode = 1

  key(KEY_W, 100, onKeyDown = if (mode == 1) _center += DVec(0, 5) else _rotation_center += DVec(0, 5))
  key(KEY_A, 100, onKeyDown = if (mode == 1) _center -= DVec(5, 0) else _rotation_center -= DVec(5, 0))
  key(KEY_S, 100, onKeyDown = if (mode == 1) _center -= DVec(0, 5) else _rotation_center -= DVec(0, 5))
  key(KEY_D, 100, onKeyDown = if (mode == 1) _center += DVec(5, 0) else _rotation_center += DVec(5, 0))

  key(KEY_1, onKeyDown = mode = 1)
  key(KEY_2, onKeyDown = mode = 2)

  key(KEY_LEFT, 100, onKeyDown = _rotation_angle_deg -= 1)
  key(KEY_RIGHT, 100, onKeyDown = _rotation_angle_deg += 1)

  key(KEY_Q, onKeyDown = {
    if (keyPressed(KEY_LCONTROL)) stopApp()
  })

  def myAbsCoord(coord: DVec) = {
    val x = (coord / globalScale) + (_center - windowCenter / globalScale)
    val a = rotationAngleDeg
    if (a != 0) {
      val p = _rotation_center
      (x - p).rotateDeg(-a) + p
    } else {
      x
    }
  }

  private var prev_m = Vec.zero
  private var prev_mouse_coord = mouse_coord
  leftMouse(onBtnDown = m => {
    mouse_coord = myAbsCoord(m)
    println(s"$m : ${m - prev_m} : $mouse_coord : ${mouse_coord - prev_mouse_coord} : ${((m - prev_m) / globalScale).toDVec == mouse_coord - prev_mouse_coord}")
    prev_m = m
    prev_mouse_coord = mouse_coord
  })

  mouseWheelUp(onWheelUp = m => {
    if (globalScale < 1) globalScale += 0.1
    else if (globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if (globalScale > 1) globalScale -= 1
    else if (globalScale > 0.1) globalScale -= 0.1
  })

  center = _center - _base
  rotationCenter = _rotation_center - _base
  rotationAngleDeg = _rotation_angle_deg
  render {
    openglLocalTransform {
      (-1005 to 1005 by 15).foreach(x => drawLine(DVec(x, -1005), DVec(x, 1005), DARK_GRAY))
      (-1005 to 1005 by 15).foreach(y => drawLine(DVec(-1005, y), DVec(1005, y), DARK_GRAY))
    }

    drawFilledCircle(mouse_coord - _base, 3, RED)

    /*drawFilledCircle(absCoord(mouse_coord), 3, GREEN)*/
    drawFilledCircle(_rotation_center - _base, 3, YELLOW)
    drawFilledCircle(_center - _base, 3, BLUE)

    openglLocalTransform {
      openglMove(_center - _base)
      openglRotateDeg(90)
      drawEllipse(DVec.zero, 100, 50, WHITE)
    }
  }

  interface {
    currentColor = WHITE
    print(s"mode: $mode", 20, 60)
    print(s"coord: $mouse_coord", 20, 40)
    print(s"coord: ${mouse_coord - _center}", 20, 20)
    /*print(s"scaled coord: ${absCoord(mouse_coord)}", 20, 20)*/
  }

  /*private def nearestPoint(v:Vec):Vec = {
    DVec(v.ix/15*15-5, v.iy/15*15-5)
  }*/
}
