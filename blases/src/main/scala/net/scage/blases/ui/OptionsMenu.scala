package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.handlers.controller2.MultiController
import net.scage.Screen

object OptionsMenu extends Screen("Blases Options") with MultiController {
  val screen_button = new Button("Screen", Vec(512, 384) + Vec(-40, 40), 100, OptionsMenu, ScreenOptionsMenu.run())
  val back_button = new Button("Back", Vec(512, 384) + Vec(-40, 0), 100, OptionsMenu, stop())
}
