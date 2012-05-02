package net.scageprojects.icebreaker

import net.scage.support.Vec
import net.scage.ScageLib._
import net.scage.{ScageScreen, ScageScreenApp}

sealed abstract class Instruction(val name:String) {
  def description:String
  def action(address:Int, A:Int, B:Int)
  override def toString = name
}
object NOP extends Instruction("NOP") {
  def description = xml("nop.description")
  def action(address:Int, A:Int, B:Int) {}
}

class MemoryCell(var A:Int = 0, var B:Int = 0, var instruction:Instruction = NOP) extends ScageScreen("Memory Cell") {

}

object WinterMute extends ScageScreenApp("WinterMute", 800, 700) {
  val empty_cell = new MemoryCell()
  val memory = Array.fill(35*35)(empty_cell)

  private var selector = Vec.zero
  key(KEY_UP,    onKeyDown = {selector += Vec(0, 1);  if(selector.y >= 35) selector = Vec(selector.x, 0)})
  key(KEY_DOWN,  onKeyDown = {selector += Vec(0, -1); if(selector.y <  0)  selector = Vec(selector.x, 34)})
  key(KEY_RIGHT, onKeyDown = {
    selector += Vec(1, 0)
    if(selector.x >= 35) {
      selector = Vec(0,  selector.y+1)
      if(selector.y >= 35) selector = Vec(selector.x, 0)
    }
  })
  key(KEY_LEFT,  onKeyDown = {
    selector += Vec(-1, 0)
    if(selector.x <  0) {
      selector = Vec(34, selector.y-1)
      if(selector.y <  0)  selector = Vec(selector.x, 34)
    }
  })
  
  render {
    for {
      x <- 0 until 35
      y <- 0 until 35
      cell = memory(35*y + x)
      cell_coord = Vec(x*20+10, y*20+10)
    } {
      if(cell == empty_cell) drawFilledRectCentered(cell_coord, 18, 18, DARK_GRAY)
      else drawFilledRectCentered(cell_coord, 18, 18, RED)
    }
    drawRectCentered(selector*20 + Vec(10, 10), 20, 20, WHITE)

    val address = (35*selector.y + selector.x).toInt
    print(address, 710, 700-20, WHITE)
    val selected_cell = memory(address)
    print(selected_cell.A+" "+selected_cell.B+"\n"+selected_cell.instruction, 710, 700-40, WHITE)
  }
}
