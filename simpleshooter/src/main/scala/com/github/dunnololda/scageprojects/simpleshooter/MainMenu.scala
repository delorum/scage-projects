package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object MainMenu extends ScageScreenApp("Simple Shooter", map_width, map_height) {
  private val title_printer = new ScageMessage(max_font_size = 50)

  private val menu_items:List[(String, Vec, List[Vec],        () => Any)] = createMenuItems(List(
    ("Создать",      Vec(windowWidth/2, windowHeight/2 + 30), () => new TacticShooterClient(None).run()),
    ("Подключиться", Vec(windowWidth/2, windowHeight/2),      () => new GamesListScreen().run()),
    ("Настройки",    Vec(windowWidth/2, windowHeight/2-30),   () => Unit),
    ("Справка",      Vec(windowWidth/2, windowHeight/2-30*2), () => Unit),
    ("Выход",        Vec(windowWidth/2, windowHeight/2-30*3), () => stop())
  ))

  leftMouse(onBtnDown = m => {
    menu_items.find(x => mouseOnArea(x._3)) match {
      case Some((_, _, _, action)) => action()
      case None =>
    }
  })

  interface {
    title_printer.print("Простая стрелялка", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    menu_items.foreach {
      case (title, coord, _, _) =>
        print(title, coord, WHITE, align = "center")
    }
    print(s"v$appVersion", 30, 20, WHITE, align = "center")
  }
}
