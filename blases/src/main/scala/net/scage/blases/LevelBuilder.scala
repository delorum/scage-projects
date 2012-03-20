package net.scage.blases

import net.scage.ScageLib._
import net.scage.ScageScreenApp
import net.scage.support.Vec
import collection.mutable.ArrayBuffer

object LevelBuilder extends ScageScreenApp("LevelBuilder") {
  private var coord1:Vec = Vec.zero
  
  private val lines = ArrayBuffer[Vec]()
  
  leftMouse(onBtnDown = {
    mouse_coord => 
      if(coord1 == Vec.zero) coord1 = mouse_coord
      else {
        lines ++= List(coord1.copy, mouse_coord.copy)
        coord1 = Vec.zero
      }
  })  
  
  render {
    if(coord1 != Vec.zero) drawLine(coord1, mouseCoord, GREEN)
    drawLines(lines.toArray, GREEN)
  }

  interface {
    print(mouseCoord, 20, 20, GREEN)
  }

  render {
    drawPolygon(Vec(84,  212), Vec(458, 564), Vec(627, 393), Vec(591, 359), Vec(454, 495), Vec(113, 175))
    drawPolygon(Vec(76,  85),  Vec(810, 83),  Vec(812,46),   Vec(77,  45))
    drawPolygon(Vec(782, 658), Vec(829, 658), Vec(830,151),  Vec(787, 152))
    drawPolygon(Vec(562, 281), Vec(615, 256), Vec(644,186),  Vec(568, 150), Vec(536, 223))
  }
}
