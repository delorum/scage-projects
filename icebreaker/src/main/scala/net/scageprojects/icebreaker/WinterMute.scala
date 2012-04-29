package net.scageprojects.icebreaker

import net.scage.ScageScreenApp
import net.scage.support.Vec
import net.scage.ScageLib._

case class MemoryCell(var A:Int = 0, var B:Int = 0, var instruction:String = "")

object WinterMute extends ScageScreenApp("WinterMute", 700, 700) {
  val empty_cell = MemoryCell()
  val memory = Array.fill(35)(Array.fill(35)(empty_cell))

  private var selector = Vec.zero
  key(KEY_UP,    onKeyDown = {selector += Vec(0, 1);  if(selector.y >= 35) selector = Vec(selector.x, 0)})
  key(KEY_DOWN,  onKeyDown = {selector += Vec(0, -1); if(selector.y <  0)  selector = Vec(selector.x, 34)})
  key(KEY_RIGHT, onKeyDown = {selector += Vec(1, 0);  if(selector.x >= 35) selector = Vec(0,  selector.y)})
  key(KEY_LEFT,  onKeyDown = {selector += Vec(-1, 0); if(selector.x <  0)  selector = Vec(34, selector.y)})
  
  render {
    for {
      x <- 0 until 35
      y <- 0 until 35
      cell = memory(x)(y)
      cell_coord = Vec(x*20+10, y*20+10)
    } {
      if(cell == empty_cell) drawFilledRectCentered(cell_coord, 18, 18, DARK_GRAY)
      else drawFilledRectCentered(cell_coord, 18, 18, RED)
    }
    drawRectCentered(selector*20 + Vec(10, 10), 20, 20, WHITE)
  }
}
