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

class MemoryCell(val address:Int, var A:Int = 0, var B:Int = 0, var I:Instruction = NOP) extends ScageScreen("Memory Cell") {
  val OPERAND_A_SELECTED   = 0
  val OPERAND_B_SELECTED   = 1
  val INSTRUCTION_SELECTED = 2

  private var selector = -1
  private var numeric_position = 0  // 0, 1, 2, 3
  init {
    selector = -1
    numeric_position = 0
  }

  key(KEY_UP,    onKeyDown = {selector += 1; if(selector >= 3) {selector = 0}; numeric_position = 0})
  key(KEY_DOWN,  onKeyDown = {selector -= 1; if(selector <  0) {selector = 2}; numeric_position = 0})
  key(KEY_RIGHT, onKeyDown = {selector += 1; if(selector >= 3) {selector = 0}; numeric_position = 0})
  key(KEY_LEFT,  onKeyDown = {selector -= 1; if(selector <  0) {selector = 2}; numeric_position = 0})

  key(KEY_ESCAPE, onKeyDown = stop())

  private def digits(number:Int) = {
    val a1 = number/1000
    val a2 = (number - a1*1000)/100
    val a3 = (number - a1*1000 - a2*100)/10
    val a4 = number - a1*1000 - a2*100 - a3*10
    Array(a1, a2, a3, a4)
  }

  private def fourDigitsString(i:Int) = {
    digits(i).mkString
  }

  private def numberFromDigits(digits:Array[Int]) = {
    val powers = (0 until digits.length).map(digits.length - 1 - _)
    digits.zip(powers).foldLeft(0) {
      case (answer, (digit,power)) => answer + (digit*math.pow(10, power)).toInt
    }
  }

  render {
    printCentered("Address", 400, 720, WHITE)
    printCentered(address,   400, 700, WHITE)

    printCentered("A",       200, 620, WHITE)
    printCentered(fourDigitsString(A),         200, 600, WHITE)

    printCentered("B",       600, 620, WHITE)
    printCentered(fourDigitsString(B),         600, 600, WHITE)

    printCentered("I",       400, 220, WHITE)
    printCentered(I,         400, 200, WHITE)

    selector match {
      case OPERAND_A_SELECTED =>
        val Vec(w, h) = messageBounds(fourDigitsString(A))
        drawRectCentered(Vec(200, 600), w, h, RED)

      case OPERAND_B_SELECTED =>
        val Vec(w, h) = messageBounds(fourDigitsString(B))
        drawRectCentered(Vec(600, 600), w, h, RED)
      case INSTRUCTION_SELECTED =>
        val Vec(w, h) = messageBounds(I)
        drawRectCentered(Vec(400, 200), w, h, RED)
      case _ =>
    }
  }

  def changeDigit(i:Int) {
    selector match {
      case OPERAND_A_SELECTED =>
        val A_digits = digits(A)
        A_digits(numeric_position) = i
        A = numberFromDigits(A_digits)
        numeric_position += 1
        if(numeric_position >= 4) numeric_position = 0
      case OPERAND_B_SELECTED =>
        val B_digits = digits(B)
        B_digits(numeric_position) = i
        B = numberFromDigits(B_digits)
        numeric_position += 1
        if(numeric_position >= 4) numeric_position = 0
      case _ =>
    }
  }

  key(KEY_1, onKeyDown = {changeDigit(1)})
  key(KEY_2, onKeyDown = {changeDigit(2)})
  key(KEY_3, onKeyDown = {changeDigit(3)})
  key(KEY_4, onKeyDown = {changeDigit(4)})
  key(KEY_5, onKeyDown = {changeDigit(5)})
  key(KEY_6, onKeyDown = {changeDigit(6)})
  key(KEY_7, onKeyDown = {changeDigit(7)})
  key(KEY_8, onKeyDown = {changeDigit(8)})
  key(KEY_9, onKeyDown = {changeDigit(9)})
  key(KEY_0, onKeyDown = {changeDigit(0)})

  key(KEY_NUMPAD1, onKeyDown = {changeDigit(1)})
  key(KEY_NUMPAD2, onKeyDown = {changeDigit(2)})
  key(KEY_NUMPAD3, onKeyDown = {changeDigit(3)})
  key(KEY_NUMPAD4, onKeyDown = {changeDigit(4)})
  key(KEY_NUMPAD5, onKeyDown = {changeDigit(5)})
  key(KEY_NUMPAD6, onKeyDown = {changeDigit(6)})
  key(KEY_NUMPAD7, onKeyDown = {changeDigit(7)})
  key(KEY_NUMPAD8, onKeyDown = {changeDigit(8)})
  key(KEY_NUMPAD9, onKeyDown = {changeDigit(9)})
  key(KEY_NUMPAD0, onKeyDown = {changeDigit(0)})
}

object WinterMute extends ScageScreenApp("WinterMute", 800, 800) {
  val memory = Array(((0 until 35*35).map(i => new MemoryCell(address = i))):_*)

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
  key(KEY_RETURN, onKeyDown = {memory((35*selector.y + selector.x).toInt).run()})
  
  render {
    for {
      x <- 0 until 35
      y <- 0 until 35
      cell = memory(35*y + x)
      cell_coord = Vec(x*20+10, y*20+10 + 100)
    } {
      if(cell.I == NOP) drawFilledRectCentered(cell_coord, 18, 18, DARK_GRAY)
      else drawFilledRectCentered(cell_coord, 18, 18, RED)
    }
    drawRectCentered(selector*20 + Vec(10, 10 + 100), 20, 20, WHITE)

    val address = (35*selector.y + selector.x).toInt
    printCentered(address, 750, 700-20 + 100, WHITE)
    val selected_cell = memory(address)
    printCentered(selected_cell.A+" "+selected_cell.B, 750, 700-60 + 100, WHITE)
    printCentered(selected_cell.I, 750, 700-80 + 100, WHITE)
  }
}

import WinterMute._

object MAA extends Instruction("MAA") {
  def description = xml("maa.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A = memory(address+A).A
  }
}

object MBB extends Instruction("MBB") {
  def description = xml("mbb.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B = memory(address+A).B
  }
}

object MAB extends Instruction("MAB") {
  def description = xml("mab.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A = memory(address+A).A
    memory(address+B).B = memory(address+A).B
  }
}

object MBA extends Instruction("MBA") {
  def description = xml("mba.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A = memory(address+A).B
    memory(address+B).B = memory(address+A).A
  }
}

object MOI extends Instruction("MOI") {
  def description = xml("moi.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).I = memory(address+A).I
  }
}

object MNA extends Instruction("MNA") {
  def description = xml("mna.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A = A
  }
}

object MNB extends Instruction("MNB") {
  def description = xml("mnb.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B = A
  }
}

object MOV extends Instruction("MOV") {
  def description = xml("mov.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A = memory(address+A).A
    memory(address+B).B = memory(address+A).B
    memory(address+B).I = memory(address+A).I
  }
}

object AAA extends Instruction("AAA") {
  def description = xml("aaa.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A += memory(address+A).A
  }
}

object ABB extends Instruction("ABB") {
  def description = xml("abb.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B += memory(address+A).B
  }
}

object AAB extends Instruction("AAB") {
  def description = xml("aab.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B += memory(address+A).A
  }
}

object ABA extends Instruction("ABA") {
  def description = xml("aba.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A += memory(address+A).B
  }
}

object ANA extends Instruction("ANA") {
  def description = xml("ana.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A += A
  }
}

object ANB extends Instruction("ANB") {
  def description = xml("anb.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B += A
  }
}

object ADD extends Instruction("ADD") {
  def description = xml("add.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A += memory(address+A).A
    memory(address+B).B += memory(address+A).B
  }
}

object SAA extends Instruction("SAA") {
  def description = xml("saa.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A -= memory(address+A).A
  }
}

object SBB extends Instruction("SBB") {
  def description = xml("sbb.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B -= memory(address+A).B
  }
}

object SAB extends Instruction("SAB") {
  def description = xml("sab.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B -= memory(address+A).A
  }
}

object SBA extends Instruction("SBA") {
  def description = xml("sba.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A -= memory(address+A).B
  }
}

object SNA extends Instruction("SNA") {
  def description = xml("sna.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A -= A
  }
}

object SNB extends Instruction("SNB") {
  def description = xml("snb.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).B -= A
  }
}

object SUB extends Instruction("SUB") {
  def description = xml("sub.description")
  def action(address:Int, A:Int, B:Int) {
    memory(address+B).A -= memory(address+A).A
    memory(address+B).B -= memory(address+A).B
  }
}