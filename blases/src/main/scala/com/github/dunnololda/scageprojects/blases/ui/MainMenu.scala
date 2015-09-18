package com.github.dunnololda.scageprojects.blases.ui

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.Blases

import scala.collection.mutable.ArrayBuffer

object MainMenu extends /*Scage*/ScreenApp("Blases", 640, 480) with MultiController {
  backgroundColor = WHITE

  val new_game_button = new Button(xml("button.newgame"), Vec(512, 384) + Vec(-40, 40), 100, MainMenu, {
    Blases.run()
    backgroundColor = WHITE
  })
  val levels_button  = new Button(xml("button.levels"), Vec(512, 384) + Vec(-40, 0), 100, MainMenu, LevelsMenu.run())
  val options_button = new Button(xml("button.options"), Vec(512, 384) + Vec(-40, -40), 100, MainMenu, OptionsMenu.run())
  val help_button    = new Button(xml("button.help"), Vec(512, 384) + Vec(-40, -80), 100, MainMenu, HelpMenu.run())
  val exit_button    = new Button(xml("button.exit"), Vec(512, 384) + Vec(-40, -120), 100, MainMenu, stopApp())

  interface {
    print("v"+appVersion, 10, 10, BLACK)
  }
  
  private val screens = ArrayBuffer[Screen]()
  def runNextScreen(screen:Screen) {
    screens += screen
  }
  action {
    if(screens.nonEmpty) {
      val next_screen = screens.remove(0)
      next_screen.run()
      backgroundColor = WHITE
    }
  }
}
