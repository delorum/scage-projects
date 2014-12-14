package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

object AngleTests extends ScageScreenApp("Angle Tests", 640, 480) {
  private var velocity = Vec(-5, 5)
  private var ship = Vec(0, 5)

  implicit class MyVec(v1:Vec) {
    def mydeg(v2:Vec):Float = {
      val scalar = v1*v2.perpendicular
      if(scalar >= 0) v1.deg(v2) else 360 - v1.deg(v2)
    }
  }

  def howManyTacts(to:Float, from:Float, a:Float, dt:Float, tacts:Int = 0):Int = {
    if(from >= to) tacts
    else howManyTacts(to, from + a*dt, dt, tacts+1)
  }


  leftMouse(onBtnDown = m => {
    velocity = m - windowCenter
  })
  rightMouse(onBtnDown = m => {
    ship = m - windowCenter
  })

  render {
    drawLine(windowCenter, windowCenter+Vec(0,100), WHITE)
    drawLine(windowCenter, windowCenter + Vec(0, 100).perpendicular, WHITE)
    drawLine(windowCenter, windowCenter+velocity.n*100, GREEN)
    drawLine(windowCenter, windowCenter+ship.n*100, YELLOW)
  }

  interface {
    val rotation = ship.mydeg(Vec(0,1))

    print(s"velocity.mydeg(Vec(0,1)) : ${velocity.mydeg(Vec(0,1))}", 20, 200, WHITE)
    print(s"Vec(0,1).mydeg(velocity) : ${Vec(0,1).mydeg(velocity)}", 20, 180, WHITE)

    print("", 20, 160, WHITE)

    print(s"ship.mydeg(Vec(0,1)) : ${ship.mydeg(Vec(0,1))}", 20, 140, WHITE)
    print(s"Vec(0,1).mydeg(ship) : ${Vec(0,1).mydeg(ship)}", 20, 120, WHITE)

    print("", 20, 100, WHITE)

    print(s"velocity.mydeg(ship) : ${velocity.mydeg(ship)}", 20, 80, WHITE)
    print(s"ship.mydeg(velocity) : ${ship.mydeg(velocity)}", 20, 60, WHITE)

    print("", 20, 40, WHITE)

    print(s"velocity.mydeg(Vec(0,1)) - rotation : ${velocity.mydeg(Vec(0,1)) - rotation}", 20, 20, WHITE)
  }
}
