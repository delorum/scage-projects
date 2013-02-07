package net.scageprojects.liftdriver.tests

import net.scage.ScageScreenApp
import net.scage.ScageLib._

object AreaForMessageTest extends ScageScreenApp(width = 640, height = 480) {
  val str = "Hello World!"
  val str2 =  "Hello World!\nHello World!\nHello World!\nHello World!"

  val align = "right"  // center, xcenter, right

  interface {
    print(str2, windowCenter, color = WHITE, align = "ycenter")
    drawFilledCircle(windowCenter, 3, color = RED)

    /*print(str2, windowCenter - Vec(0,60), align = "xcenter")
    drawSlidingLines(areaForMessage(windowCenter - Vec(0,60), str2, align = "xcenter"))

    print(str2, windowCenter - Vec(0,120), align = "right")
    drawSlidingLines(areaForMessage(windowCenter - Vec(0,120), str2, align = "right"))*/
  }
}
