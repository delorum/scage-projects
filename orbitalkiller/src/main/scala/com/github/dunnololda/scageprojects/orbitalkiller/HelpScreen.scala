package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

object HelpScreen extends ScageScreen("Help Screen") {
  val s =
    """|Клавиши цифровой клавиатуры (Num Lock = on) - выбор двигателей:
      |1,4,7 - двигатели левой стороны
      |3,6,9 - двигатели правой стороны
      |2,8 - нижний и верхний двигатели
      |5 - отключить все двигатели, сбросить мощность до нуля
      |
      |+, - - ускорение/замедление времени
      |
      |Стрелки вверх/вниз - добавить/убавить мощность выбранного двигателя
      |Стрелки влево/вправо - добавить/убавить время работы выбранного двигателя
      |
      |W, A, S, D - перемещение точки обзора
      |Колесо мышки - приближение/отдаление
      |Пробел - Ориентировать корабль по центру окна
      |P - Пауза
      |
      |M - Режим карты
      |В режиме карты перемещение мышки с нажатой левой кнопкой рисует рамку
      |Отпустить кнопку: область внутри рамки будет смасштабирована на весь экран
      |
      |F2 - Камера, фиксация на корабле
      |F3 - Камера, фиксация на корабле, абсолютная ориентация
      |
      |1 - Режим полета, свободный
      |2 - Режим полета, запрет вращения
      |3 - Режим полета, ориентация по осям
      |4 - Режим полета, ориентация по траектории
      |5 - Режим полета, ориентация против траектории траектории
      |6 - Режим полета, выход на круговую орбиту
      |7 - Режим полета, уравнять скорость с ближайшим кораблем
      |
      |V - рисовать/не рисовать интерфейс
      |
      |Нажмите Escape
    """.stripMargin

  interface {
    print(s, 20, windowHeight - 30, color = WHITE)
  }

  key(KEY_ESCAPE, onKeyDown = stop())
  keyIgnorePause(KEY_Q, onKeyDown = {
    if (keyPressed(KEY_LCONTROL)) stopApp()
  })
}
