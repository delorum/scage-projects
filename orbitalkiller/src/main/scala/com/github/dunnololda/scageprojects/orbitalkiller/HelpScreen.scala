package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

object HelpScreen extends ScageScreen("Help Screen") {
  val s =
    """
      |Клавиши цифровой клавиатуры (Num Lock = on):
      |1,4,7 - двигатели левой стороны
      |3,6,9 - двигатели правой стороны
      |2,8 - нижний и верхний двигатели
      |
      |W,A,S,D - управление камерой
      |Колесо мышки - приближение/отдаление
      |Пробел - Ориентировать корабль по центру окна
      |P - Пауза
      |F1 - Помощь
      |
      |Нажмите Escape
    """.stripMargin

  interface {
    print(s, 20, windowHeight - 30, color = WHITE)
  }

  key(KEY_ESCAPE, onKeyDown = stop())
}
