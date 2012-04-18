package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.blases.Blases
import net.scage.ScageLib._
import net.scage.handlers.controller2.MultiController
import net.scage.{Screen, ScreenApp, Scage, ScageScreenApp}
import collection.mutable.ArrayBuffer

object MainMenu extends ScreenApp("Blases", 640, 480) with MultiController {
  backgroundColor = WHITE

  val new_game_button = new Button(xml("button.newgame"), Vec(512, 384) + Vec(-40, 40), 100, MainMenu, {
    Blases.run()
    backgroundColor = WHITE
  })
  val levels_button  = new Button(xml("button.levels"), Vec(512, 384) + Vec(-40, 0), 100, MainMenu, LevelsMenu.run())
  val options_button = new Button(xml("button.options"), Vec(512, 384) + Vec(-40, -40), 100, MainMenu, OptionsMenu.run())
  val help_button    = new Button(xml("button.help"), Vec(512, 384) + Vec(-40, -80), 100, MainMenu, HelpMenu.run())
  val exit_button    = new Button(xml("button.exit"), Vec(512, 384) + Vec(-40, -120), 100, MainMenu, Scage.stopApp())

  interface {
    print("v"+app_version, 10, 10, BLACK)
  }
  
  private val screens = ArrayBuffer[Screen]()
  def runNextScreen(screen:Screen) {
    screens += screen
  }
  action {
    if(!screens.isEmpty) {
      val next_screen = screens.remove(0)
      next_screen.run()
      backgroundColor = WHITE
    }
  }
}
