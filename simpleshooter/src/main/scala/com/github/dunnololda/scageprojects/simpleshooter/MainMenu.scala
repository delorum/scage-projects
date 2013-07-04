package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object MainMenu extends ScageScreenApp("Simple Shooter", default_window_width, default_window_height) {
  private val menu_items = createMenuItems(List(
    ("Создать",      () => Vec(windowWidth/2, windowHeight/2 + 30), () => WHITE, () => new TacticShooterClient(None).run()),
    ("Подключиться", () => Vec(windowWidth/2, windowHeight/2),      () => WHITE, () => new GamesListScreen().run()),
    ("Обучение",     () => Vec(windowWidth/2, windowHeight/2-30),   () => WHITE, () => new TutorialScreen().run()),
    ("Настройки",    () => Vec(windowWidth/2, windowHeight/2-30*2), () => WHITE, () => SettingsScreen.run()),
    ("Справка",      () => Vec(windowWidth/2, windowHeight/2-30*3), () => WHITE, () => HelpScreen.run()),
    ("Выход",        () => Vec(windowWidth/2, windowHeight/2-30*4), () => WHITE, () => stop())
  ))

  private var selected_menu_item:Option[Int] = None
  private def menuItemColor(idx:Int, color:ScageColor):ScageColor = {
    selected_menu_item match {
      case Some(selected_idx) if idx == selected_idx => RED
      case _ => color
    }
  }

  key(KEY_ESCAPE, onKeyDown = stop())

  leftMouse(
    onBtnDown = m => {
      menu_items.zipWithIndex.find(x => mouseOnArea(x._1._3())) match {
        case Some((pewpew, idx)) => selected_menu_item = Some(idx)
        case None =>
      }
    },
    onBtnUp = m => {
      selected_menu_item.foreach(idx => menu_items(idx)._5())
      selected_menu_item = None
    }
  )

  interface {
    main_title_printer.print("Простая стрелялка", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    menu_items.zipWithIndex.foreach {
      case ((title, coord, _, color, _), idx) =>
        print(title, coord(), menuItemColor(idx, color()), align = "center")
    }
    print(s"v$appVersion", 30, 20, WHITE, align = "center")
  }
}
