package com.github.dunnololda.scageprojects.td.tests

import com.github.dunnololda.scage.ScageLib._

object MessageBoundsTest extends ScageScreenApp("Message Bounds Test", 640 ,480) {
  val pew = "HP: 45"+"\n"+"A: 6.6"+"\n"+"Upgrade"+"\n"+"Repair"
  val pewpew = messageBounds(pew)
  println(pewpew)

  //backgroundColor = WHITE
  render {
    printCentered(pew, windowCenter)
    drawRectCentered(windowCenter, pewpew.x, pewpew.y, RED)
    drawRectCentered(windowCenter, 52, 88, YELLOW)
  }
}
