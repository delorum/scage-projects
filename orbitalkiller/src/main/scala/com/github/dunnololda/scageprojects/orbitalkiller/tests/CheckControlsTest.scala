package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

object CheckControlsTest extends ScageScreenAppMT("Check Controls Test", 640, 480) {
  private val buf = ArrayBuffer[String]()

  private def add(str: String) {
    buf += str
    if (buf.length > 10) buf.remove(0)
  }

  private val mouse_buf = ArrayBuffer[Vec]()

  key(KEY_1, onKeyDown = add("1"))
  key(KEY_2, onKeyDown = add("2"))
  key(KEY_3, onKeyDown = add("3"))
  key(KEY_4, onKeyDown = add("4"))
  key(KEY_5, onKeyDown = add("5"))
  key(KEY_6, onKeyDown = add("6"))
  key(KEY_7, onKeyDown = add("7"))
  key(KEY_8, onKeyDown = add("8"))
  key(KEY_9, onKeyDown = add("9"))
  key(KEY_0, onKeyDown = add("0"))

  key(KEY_Q, onKeyDown = if (keyPressed(KEY_LCONTROL)) stopApp())

  leftMouse(onBtnDown = mc => {
    mouse_buf += mc
    if (mouse_buf.length > 10) mouse_buf.remove(0)
  })

  action {
    //Thread.sleep(100)
  }

  render {
    mouse_buf.foreach {
      case mc => drawFilledCircle(mc, 3, WHITE)
    }

    drawLine(windowCenter, mouseCoord, WHITE)
  }

  interface {
    print(s"FPS/Ticks $fps/$ticks",
      windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec * fps / (averageRenderTimeMsec * fps + averageActionTimeMsec * ticks) * 100}%.2f%%/${1 * averageActionTimeMsec * ticks / (averageRenderTimeMsec * fps + averageActionTimeMsec * ticks) * 100}%.2f%%",
      windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec",
      windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec",
      windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(buf.mkString(" "), 20, windowHeight / 2, color = WHITE, align = "center-left")
  }
}
