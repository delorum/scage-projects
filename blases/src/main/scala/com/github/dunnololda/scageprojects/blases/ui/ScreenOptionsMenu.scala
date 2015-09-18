package com.github.dunnololda.scageprojects.blases.ui

object ScreenOptionsMenu extends /*Scage*/Screen("Blases Options Screen") with MultiController {
  val resolution1_button = new Button("640 x 480", Vec(512, 384) + Vec(-40, 40), 100, ScreenOptionsMenu, windowSize = (640, 480))
  val resolution2_button = new Button("800 x 600", Vec(512, 384) + Vec(-40, 0), 100, ScreenOptionsMenu, windowSize = (800, 600))
  val resolution3_button = new Button("1024 x 768", Vec(512, 384) + Vec(-40, -40), 100, ScreenOptionsMenu, windowSize = (1024, 768))
  val back_button = new Button(xml("button.back"), Vec(512, 384) + Vec(-40, -80), 100, ScreenOptionsMenu, stop())
}
