package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object SettingsScreen extends ScageScreen("Settings Screen") {
  private val menu_items:List[(String, () => Vec, () => List[Vec], ScageColor, () => Any)] = createMenuItems(List(
    ("Разрешение", () => Vec(windowWidth/2, windowHeight/2),      WHITE,     () => WindowSettingsScreen.run()),
    ("Язык",       () => Vec(windowWidth/2, windowHeight/2-30),   DARK_GRAY, () => Unit),
    ("Назад",      () => Vec(windowWidth/2, windowHeight/2-30*2), WHITE,     () => stop())
  ))

  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(onBtnDown = m => {
    menu_items.find(x => mouseOnArea(x._3())) match {
      case Some((_, _, _, _, action)) => action()
      case None =>
    }
  })

  interface {
    settings_title_printer.print("Настройки", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    menu_items.foreach {
      case (title, coord, _, color, _) =>
        print(title, coord(), color, align = "center")
    }
  }
}
