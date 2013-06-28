package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object WindowSettingsScreen extends ScageScreen("Window Settings Screen") {
  private def newWindowSize(w:Int, h:Int) {
    windowSize = (w, h)
    main_title_printer.reloadFont()
    settings_title_printer.reloadFont()
    window_settings_title_printer.reloadFont()
    help_printer.reloadFont()
  }

  private val menu_items:List[(String, () => Vec, () => List[Vec], ScageColor, () => Any)] = createMenuItems(List(
    ("1024x768", () => Vec(windowWidth/2, windowHeight/2),      WHITE, () => newWindowSize(1024, 768)),
    ("800x600",  () => Vec(windowWidth/2, windowHeight/2-30),   WHITE, () => newWindowSize(800, 600)),
    ("640x480",  () => Vec(windowWidth/2, windowHeight/2-30*2), WHITE, () => newWindowSize(640, 480)),
    ("Назад",    () => Vec(windowWidth/2, windowHeight/2-30*3), WHITE, () => stop())
  ))

  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(onBtnDown = m => {
    menu_items.find(x => mouseOnArea(x._3())) match {
      case Some((_, _, _, _, action)) => action()
      case None =>
    }
  })

  interface {
    window_settings_title_printer.print("Настройки разрешения", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    menu_items.foreach {
      case (title, coord, _, color, _) =>
        print(title, coord(), color, align = "center")
    }
  }
}
