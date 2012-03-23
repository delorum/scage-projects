package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.blases.ui.Button
import net.scage.blases.{Blases, Button}
import net.scage.{Scage, ScageScreenApp}
import net.scage.ScageLib._

object MainMenu extends ScageScreenApp("Blases Main Menu", 640, 480, "Blases") {
  backgroundColor = WHITE

  val new_game_button = new Button("New Game", Vec(512, 384) + Vec(-40, 40), 100, MainMenu)
  val options_button = new Button("Options", Vec(512, 384) + Vec(-40, 0), 100, MainMenu)
  val exit_button = new Button("Exit", Vec(512, 384) + Vec(-40, -40), 100, MainMenu)

  leftMouse(onBtnDown = {
    m =>
      if (new_game_button.containsCoord(m)) {
        Blases.run()
        backgroundColor = WHITE
      } else if (options_button.containsCoord(m)) {
        OptionsMenu.run()
      } else if (exit_button.containsCoord(m)) {
        Scage.stopApp()
      }
  })
}
