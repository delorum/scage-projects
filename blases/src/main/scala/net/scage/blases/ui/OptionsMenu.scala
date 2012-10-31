package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.handlers.controller2.MultiController
import net.scage.{ScageScreen, Screen}
import net.scage.ScageLib._

object OptionsMenu extends /*Scage*/Screen("Blases Options") with MultiController {
  val screen_button = new Button(xml("button.screen"), Vec(512, 384) + Vec(-40, 40), 100, OptionsMenu, ScreenOptionsMenu.run())
  val language_button = new Button(xml("button.language"), Vec(512, 384) + Vec(-40, 0), 100, OptionsMenu, LanguageOptionsMenu.run())
  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-40, -40), 100, OptionsMenu, stop())
}
