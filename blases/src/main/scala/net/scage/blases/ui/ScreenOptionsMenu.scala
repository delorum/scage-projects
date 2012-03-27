package net.scage.blases.ui

import net.scage.ScageLib._
import net.scage.support.Vec
import MainMenu._
import net.scage.handlers.controller2.MultiController
import net.scage.{Screen, ScageScreen}

object ScreenOptionsMenu extends Screen("Blases Options Screen") with MultiController {
  val resolution1_button = new Button("640 x 480", Vec(512, 384) + Vec(-40, 40), 100, ScreenOptionsMenu, changeResolution(640, 480))
  val resolution2_button = new Button("800 x 600", Vec(512, 384) + Vec(-40, 0), 100, ScreenOptionsMenu, changeResolution(800, 600))
  val resolution3_button = new Button("1024 x 768", Vec(512, 384) + Vec(-40, -40), 100, ScreenOptionsMenu, changeResolution(1024, 768))
  val back_button = new Button("Back", Vec(512, 384) + Vec(-40, -80), 100, ScreenOptionsMenu, stop())
}
