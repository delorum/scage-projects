package net.scage.blases.ui

import net.scage.handlers.controller2.MultiController
import net.scage.support.Vec
import net.scage.ScageLib._
import net.scage.Screen
import net.scage.blases.Relatives._

object HelpMenu extends Screen("Help Menu") with MultiController {
  val back_button = new Button("Back", Vec(512, 384) + Vec(-320, -200), 100, HelpMenu, stop())

  interface {
    print("Make it from the start to the finish shooting bubbles!\n\n" +
          "Controls:\n" +
          "Left Click - shoot a new bubble or select some existing\n" +
          "Shift + Left Click - shoot a new bubble preserving the current\nbubble selection\n" +
          "Right Click - pop some bubble\n" +
          "Space - stop the current selected bubble\n" +
          "Esc - pause the game", rVec(512, 384) + rVec(-320, 160), BLACK)
  }
}
