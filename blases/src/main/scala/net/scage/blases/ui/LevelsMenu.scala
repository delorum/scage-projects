package net.scage.blases.ui

import net.scage.handlers.controller2.MultiController
import net.scage.Screen
import net.scage.support.Vec
import net.scage.blases.levels.Level1
import net.scage.ScageLib._

object LevelsMenu extends Screen("Blases Levels") with MultiController {
  val level1_button = new LevelButton(Level1, 1, Vec(512, 384) + Vec(-40, 40), LevelsMenu)
  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-40, -40), 100, LevelsMenu, stop())
}
