package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object MainMenu extends ScageScreenApp("Simple Shooter", 800, 600) {
  private val title_printer = new ScageMessage(max_font_size = 50)
  interface {
    title_printer.print("Simple Shooter", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    print("Создать", Vec(windowWidth/2, windowHeight/2 + 30), WHITE, align = "center")
    print("Подключиться", Vec(windowWidth/2, windowHeight/2), WHITE, align = "center")
    print("Настройки", Vec(windowWidth/2, windowHeight/2-30), WHITE, align = "center")
    print("Справка", Vec(windowWidth/2, windowHeight/2-30*2), WHITE, align = "center")
    print("Выход", Vec(windowWidth/2, windowHeight/2-30*3), WHITE, align = "center")
    print(s"v$appVersion", 20, 20, WHITE, align = "center")
  }

  private val create_area = messageArea("Создать", Vec(windowWidth/2, windowHeight/2 + 30))
  private val join_area = messageArea("Подключиться", Vec(windowWidth/2, windowHeight/2))
  private val exit_area = messageArea("Выход", Vec(windowWidth/2, windowHeight/2-30*3))
  leftMouse(onBtnDown = m => {
    if(mouseOnArea(create_area)) {
      new TacticShooterClient(None).run()
    } else if(mouseOnArea(join_area)) {
      new GamesListScreen().run()
    } else if(mouseOnArea(exit_area)) {
      stop()
    }
  })
}
