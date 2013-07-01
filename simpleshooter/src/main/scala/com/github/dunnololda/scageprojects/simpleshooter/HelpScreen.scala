package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._

object HelpScreen extends ScageScreen("Help Screen") {
  private val help_info =
    """
      |WASD - Перемещение поля зрения
      |
      |1,2,3 - Выбор бойца. Нажатие второй раз: сфокусировать камеру на нем
      |
      |Левый клик - выбор точки перемещения для бойца. Можно кликать несколько раз,
      |задавая траекторию
      |
      |Пробел - удалить все точки траектории. Если боец двигался, он остановится
      |
      |Правый клик - зафиксировать/освободить направление взгляда бойца
      |
      |Левый шифт - перевести режим огня в позицию выше текущей
      |(предохранитель - одиночный - автоматический)
      |
      |Левый контрол - перевести режим огня в позицию ниже текущей
      |(предохранитель - одиночный - автоматический)
      |
      |Колесико мышки - приблизить/отдалить карту
    """.stripMargin

  private val menu_items:List[(String, () => Vec, () => List[Vec], ScageColor, () => Any)] = createMenuItems(List(
    ("Назад", () => Vec(windowWidth/2, windowHeight/2-30*4), WHITE, () => stop())
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
    help_printer.print(help_info, 20, windowHeight-20, WHITE)
    menu_items.zipWithIndex.foreach {
      case ((title, coord, _, color, _), idx) =>
        print(title, coord(), menuItemColor(idx, color), align = "center")
    }
  }
}
