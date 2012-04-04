package net.scage.blases.ui

import net.scage.ScageLib._
import net.scage.support.Vec
import net.scage.handlers.controller2.MultiController
import net.scage.Screen

object LanguageOptionsMenu extends Screen("Blases Language") with MultiController {
  val russian_button = new Button(xml("button.russian"), Vec(512, 384) + Vec(-40, 40), 100, LanguageOptionsMenu, lang = "ru")
  val english_button = new Button(xml("button.english"), Vec(512, 384) + Vec(-40, 0), 100, LanguageOptionsMenu, lang = "en")
  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-40, -40), 100, LanguageOptionsMenu, stop())
}
