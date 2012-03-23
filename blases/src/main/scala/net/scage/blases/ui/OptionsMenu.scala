package net.scage.blases.ui

import net.scage.ScageScreen
import net.scage.blases.ui.Button
import net.scage.blases.Button
import net.scage.support.Vec
import net.scage.ScageLib._

object OptionsMenu extends ScageScreen("Blases Options") {
  val screen_button = new Button("Screen", Vec(512, 384) + Vec(-40, 40), 100, OptionsMenu)
  val back_button = new Button("Back", Vec(512, 384) + Vec(-40, 0), 100, OptionsMenu)

  leftMouse(onBtnDown = {
    m =>
      if (screen_button.containsCoord(m)) {
        ScreenOptionsMenu.run()
      } else if (back_button.containsCoord(m)) {
        stop()
      }
  })
}
