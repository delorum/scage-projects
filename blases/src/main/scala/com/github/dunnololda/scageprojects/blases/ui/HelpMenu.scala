package com.github.dunnololda.scageprojects.blases.ui

object HelpMenu extends /*Scage*/Screen("Help Menu") with MultiController {
  preinit {
    backgroundColor = WHITE
  }

  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-320, -200), 100, HelpMenu, stop())

  interface {
    print(xml("help.helptext"), rVec(512, 384) + rVec(-320, 160), BLACK)
  }
}
