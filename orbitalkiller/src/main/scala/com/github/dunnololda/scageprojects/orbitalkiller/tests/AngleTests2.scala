package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD.{DVec, print, _}

object AngleTests2 extends ScageScreenAppD("Angle Tests 2", 640, 480) {
  implicit class MyDVec(v1:DVec) {
    def mydeg(v2:DVec):Double = {
      val scalar = v1*v2.perpendicular
      if(scalar >= 0) v1.absDeg(v2) else 360 - v1.absDeg(v2)
    }

    def mydeg2(v2:DVec):Double = {
      val x = -v1.signedDeg(v2)
      if(x < 0) 360 + x else x
    }
  }

  private var pos = DVec(-45, 45)
  leftMouse(onBtnDown = m => {
    pos = m - windowCenter
  })

  render {
    drawLine(windowCenter, windowCenter + DVec(0, 100), WHITE)
    drawLine(windowCenter, windowCenter+pos, WHITE)
  }

  interface {
    print(s"pos.mydeg(DVec(0, 100)) == pos.mydeg2(DVec(0,100)) = ${pos.mydeg(DVec(0, 100)) == pos.mydeg2(DVec(0, 100))}", 20, 80, WHITE)
    print(s"pos.mydeg(DVec(0, 100)) = ${pos.mydeg(DVec(0, 100))}", 20, 60, WHITE)
    print(s"pos.signedDeg(DVec(0, 100)) = ${pos.signedDeg(DVec(0, 100))}", 20, 40, WHITE)
    print(s"pos.absDeg(DVec(0, 100)) = ${pos.absDeg(DVec(0, 100))}", 20, 20, WHITE)
  }
}
