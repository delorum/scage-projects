package com.github.dunnololda.scageprojects.blases.levelbuilder

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

object LevelBuilder extends ScageScreenApp("LevelBuilder") {
  private var coord1: Vec = Vec.zero

  private val lines = ArrayBuffer[Vec]()

  leftMouse(onBtnDown = {
    mouse_coord =>
      if (coord1 == Vec.zero) coord1 = mouse_coord
      else {
        lines ++= List(coord1.copy, mouse_coord.copy)
        coord1 = Vec.zero
      }
  })

  render {
    if (coord1 != Vec.zero) drawLine(coord1, mouseCoord, GREEN)
    drawLines(lines.toArray, GREEN)
  }

  interface {
    print(mouseCoord, 20, 20, GREEN)
  }

  /*val first  = new StaticPolygon(Vec(86,  526), Vec(353, 526), Vec(353, 414), Vec(86, 414))
  val second = new StaticPolygon(Vec(625,  715),  Vec(729, 715),  Vec(730, 414),   Vec(625,  414))
  val third  = new StaticPolygon(Vec(227, 311), Vec(502, 311), Vec(502, 280),  Vec(227, 280))
  val fourth = new StaticPolygon(Vec(730, 212), Vec(779, 170), Vec(779, 105),  Vec(682, 105), Vec(682, 170))
  val fifth  = new StaticPolygon(Vec(568, 143), Vec(594, 124), Vec(511, 17),  Vec(487, 38))

  render {
    currentColor = WHITE
          drawPolygon(first.points)
          drawPolygon(second.points)
          drawPolygon(third.points)
          drawPolygon(fourth.points)
          drawPolygon(fifth.points)
  }*/
}
