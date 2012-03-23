package net.scage.blases.ui

import net.scage.ScageLib._
import net.scage.support.Vec
import net.scage.ScageScreen
import MainMenu._

object ScreenOptionsMenu extends ScageScreen("Blases Options Screen") {
  val resolution1_button = new Button("640 x 480", Vec(512, 384) + Vec(-40, 40), 100, ScreenOptionsMenu)
  val resolution2_button = new Button("800 x 600", Vec(512, 384) + Vec(-40, 0), 100, ScreenOptionsMenu)
  val resolution3_button = new Button("1024 x 768", Vec(512, 384) + Vec(-40, -40), 100, ScreenOptionsMenu)
  val back_button = new Button("Back", Vec(512, 384) + Vec(-40, -80), 100, ScreenOptionsMenu)

  leftMouse(onBtnDown = {
    m =>
      if (resolution1_button.containsCoord(m)) {
        changeResolution(640, 480)
        backgroundColor = WHITE
      } else if (resolution2_button.containsCoord(m)) {
        changeResolution(800, 600)
        backgroundColor = WHITE
      } else if (resolution3_button.containsCoord(m)) {
        changeResolution(1024, 768)
        backgroundColor = WHITE
      } else if (back_button.containsCoord(m)) {
        stop()
      }
  })
}
