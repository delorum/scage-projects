package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import java.awt.GraphicsEnvironment

object WindowSettingsScreen extends ScageScreen("Window Settings Screen") {
  private def newWindowSize(w:Int, h:Int) {
    windowSize = (w, h)
    main_title_printer.reloadFont()
    settings_title_printer.reloadFont()
    window_settings_title_printer.reloadFont()
    help_printer.reloadFont()
  }

  private val gd = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  private val max_width = gd.getDisplayMode.getWidth
  private val max_height = gd.getDisplayMode.getHeight
  private val menu_items = createMenuItems(List(
    (s"На весь экран (${max_width}x${max_height})",() => Vec(windowWidth/2, windowHeight/2), () => WHITE, () => newWindowSize(max_width, max_height)),
    ("1024x768", () => Vec(windowWidth/2, windowHeight/2-30),   () => WHITE, () => newWindowSize(1024, 768)),
    ("800x600",  () => Vec(windowWidth/2, windowHeight/2-30*2), () => WHITE, () => newWindowSize(800, 600)),
    ("640x480",  () => Vec(windowWidth/2, windowHeight/2-30*3), () => WHITE, () => newWindowSize(640, 480)),
    ("Назад",    () => Vec(windowWidth/2, windowHeight/2-30*4), () => WHITE, () => stop())
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
    window_settings_title_printer.print("Настройки разрешения", Vec(windowWidth/2, windowHeight/2 + 30*4), WHITE, align = "center")
    menu_items.zipWithIndex.foreach {
      case ((title, coord, _, color, _), idx) =>
        print(title, coord(), menuItemColor(idx, color()), align = "center")
    }
  }
}
