package com.github.dunnololda.scageprojects.blases.ui

import com.github.dunnololda.scage.ScageLib._

object LanguageOptionsMenu extends /*Scage*/Screen("Blases Language") with MultiController {
  val russian_button = new Button(xml("button.russian"), Vec(512, 384) + Vec(-40, 40), 100, LanguageOptionsMenu, lang = "ru")
  val english_button = new Button(xml("button.english"), Vec(512, 384) + Vec(-40, 0), 100, LanguageOptionsMenu, lang = "en")
  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-40, -40), 100, LanguageOptionsMenu, stop())
}
