package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object MainMenu extends ScageScreenApp("Simple Shooter", game_window_width, game_window_height) {
  private val menu_items:List[(String, () => Vec, () => List[Vec], ScageColor, () => Any)] = createMenuItems(List(
    ("Создать",      () => Vec(windowWidth/2, windowHeight/2 + 30), WHITE, () => new TacticShooterClient(None).run()),
    ("Подключиться", () => Vec(windowWidth/2, windowHeight/2),      WHITE, () => new GamesListScreen().run()),
    ("Настройки",    () => Vec(windowWidth/2, windowHeight/2-30),   WHITE, () => SettingsScreen.run()),
    ("Справка",      () => Vec(windowWidth/2, windowHeight/2-30*2), WHITE, () => HelpScreen.run()),
    ("Выход",        () => Vec(windowWidth/2, windowHeight/2-30*3), WHITE, () => stop())
  ))

  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(onBtnDown = m => {
    menu_items.find(x => mouseOnArea(x._3())) match {
      case Some((_, _, _, _, action)) => action()
      case None =>
    }
  })

  interface {
    main_title_printer.print("Простая стрелялка", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    menu_items.foreach {
      case (title, coord, _, color, _) =>
        print(title, coord(), color, align = "center")
    }
    print(s"v$appVersion", 30, 20, WHITE, align = "center")
  }
}
