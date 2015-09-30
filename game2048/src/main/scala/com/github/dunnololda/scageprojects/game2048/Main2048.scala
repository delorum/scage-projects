package com.github.dunnololda.scageprojects.game2048

import com.github.dunnololda.scage.ScageLib._

object Main2048 extends ScageScreenApp("2048", 740, 480) {
  private val field = Array.ofDim[Int](4, 4)

  private var count = 0
  private var not_lost = true

  private def clearField(): Unit = {
    for {
      y <- 0 until field.length
      x <- 0 until field(y).length
    } {
      field(y)(x) = 0
    }
  }

  private def addNumber(): Unit = {
    val free_positions = field.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.flatMap {
        case (elem, x) => if(elem == 0) Some((y,x)) else None
      }
    }
    if(free_positions.isEmpty) {
      not_lost = false
      pause()
    }

    {
      val (y, x) = free_positions((math.random * free_positions.length).toInt)
      if (math.random < 0.1) field(y)(x) = 4 else field(y)(x) = 2
    }

    val new_free_positions = field.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.flatMap {
        case (elem, x) => if(elem == 0) Some((y,x)) else None
      }
    }
    if(new_free_positions.isEmpty) {
      not_lost = false
      pause()
    }
  }

  private def rowCoords(y:Int):Array[(Int,Int)] = Array((y, 0), (y, 1), (y, 2), (y, 3))
  private def columnCoords(x:Int):Array[(Int, Int)] = Array((0, x), (1, x), (2, x), (3, x))

  private def up(): Unit = {
    var move_happened = false
    val sum_happened = Array(0,0,0,0)
    def _change(fromy:Int, fromx:Int, toy:Int, tox:Int): Unit = {
      if(field(fromy)(fromx) != 0) {
        if (field(toy)(tox) == 0) {
          field(toy)(tox) = field(fromy)(fromx)
          field(fromy)(fromx) = 0
          move_happened = true
        } else {
          if(sum_happened(fromx) == 0 && field(toy)(tox) == field(fromy)(fromx)) {
            field(toy)(tox) += field(fromy)(fromx)
            count += field(toy)(tox)
            field(fromy)(fromx) = 0
            sum_happened(fromx) = 1
            move_happened = true
          }
        }
      }
    }
    def _change2(from_row:Int, to_row:Int): Unit = {
      rowCoords(from_row).zip(rowCoords(to_row)).foreach {
        case ((fromy, fromx), (toy, tox)) =>
          _change(fromy, fromx, toy, tox)
      }
    }
    _change2(1,0)
    _change2(2,1)
    _change2(1,0)
    _change2(3,2)
    _change2(2,1)
    _change2(1,0)
    if(move_happened) addNumber()
  }

  private def down(): Unit = {
    var move_happened = false
    val sum_happened = Array(0,0,0,0)
    def _change(fromy:Int, fromx:Int, toy:Int, tox:Int): Unit = {
      if(field(fromy)(fromx) != 0) {
        if (field(toy)(tox) == 0) {
          field(toy)(tox) = field(fromy)(fromx)
          field(fromy)(fromx) = 0
          move_happened = true
        } else {
          if(sum_happened(fromx) == 0 && field(toy)(tox) == field(fromy)(fromx)) {
            field(toy)(tox) += field(fromy)(fromx)
            count += field(toy)(tox)
            field(fromy)(fromx) = 0
            sum_happened(fromx) = 1
            move_happened = true
          }
        }
      }
    }
    def _change2(from_row:Int, to_row:Int): Unit = {
      rowCoords(from_row).zip(rowCoords(to_row)).foreach {
        case ((fromy, fromx), (toy, tox)) =>
          _change(fromy, fromx, toy, tox)
      }
    }
    _change2(2,3)
    _change2(1,2)
    _change2(2,3)
    _change2(0,1)
    _change2(1,2)
    _change2(2,3)
    if(move_happened) addNumber()
  }

  private def left(): Unit = {
    var move_happened = false
    val sum_happened = Array(0,0,0,0)
    def _change(fromy:Int, fromx:Int, toy:Int, tox:Int): Unit = {
      if(field(fromy)(fromx) != 0) {
        if (field(toy)(tox) == 0) {
          field(toy)(tox) = field(fromy)(fromx)
          field(fromy)(fromx) = 0
          move_happened = true
        } else {
          if(sum_happened(fromy) == 0 && field(toy)(tox) == field(fromy)(fromx)) {
            field(toy)(tox) += field(fromy)(fromx)
            count += field(toy)(tox)
            field(fromy)(fromx) = 0
            sum_happened(fromy) = 1
            move_happened = true
          }
        }
      }
    }
    def _change2(from_column:Int, to_column:Int): Unit = {
      columnCoords(from_column).zip(columnCoords(to_column)).foreach {
        case ((fromy, fromx), (toy, tox)) =>
          _change(fromy, fromx, toy, tox)
      }
    }
    _change2(1,0)
    _change2(2,1)
    _change2(1,0)
    _change2(3,2)
    _change2(2,1)
    _change2(1,0)
    if(move_happened) addNumber()
  }

  private def right(): Unit = {
    var move_happened = false
    val sum_happened = Array(0,0,0,0)
    def _change(fromy:Int, fromx:Int, toy:Int, tox:Int): Unit = {
      if(field(fromy)(fromx) != 0) {
        if (field(toy)(tox) == 0) {
          field(toy)(tox) = field(fromy)(fromx)
          field(fromy)(fromx) = 0
          move_happened = true
        } else {
          if(sum_happened(fromy) == 0 && field(toy)(tox) == field(fromy)(fromx)) {
            field(toy)(tox) += field(fromy)(fromx)
            count += field(toy)(tox)
            field(fromy)(fromx) = 0
            sum_happened(fromy) = 1
            move_happened = true
          }
        }
      }
    }
    def _change2(from_column:Int, to_column:Int): Unit = {
      columnCoords(from_column).zip(columnCoords(to_column)).foreach {
        case ((fromy, fromx), (toy, tox)) =>
          _change(fromy, fromx, toy, tox)
      }
    }
    _change2(2,3)
    _change2(1,2)
    _change2(2,3)
    _change2(0,1)
    _change2(1,2)
    _change2(2,3)
    if(move_happened) addNumber()
  }

  private val numbers_3char_printer = new ScageMessage(max_font_size = 70)
  private val numbers_4char_printer = new ScageMessage(max_font_size = 60)
  private val numbers_5char_printer = new ScageMessage(max_font_size = 50)
  private val numbers_6char_printer = new ScageMessage(max_font_size = 40)

  init {
    count = 0
    not_lost = true
    clearField()
    addNumber()
    addNumber()
    pauseOff()
  }

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp()})
  keyIgnorePause(KEY_F2, onKeyDown = restart())

  key(KEY_UP,    onKeyDown = up())
  key(KEY_DOWN,  onKeyDown = down())
  key(KEY_RIGHT, onKeyDown = right())
  key(KEY_LEFT,  onKeyDown = left())

  render {
    val color = if(not_lost) WHITE else DARK_GRAY
    (0 to 4).foreach(y => {
      drawLine(Vec(20, 480-20-y*(480-40)/4), Vec(640-20, 480-20-y*(480-40)/4), color)
    })

    (0 to 4).foreach(x => {
      drawLine(Vec(20+x*(640-40)/4, 20), Vec(20+x*(640-40)/4, 480-20), color)
    })

    for {
      (row, y) <- field.zipWithIndex
      (elem, x) <- row.zipWithIndex
      if elem != 0
    } {
      if(elem < 1000) {
        numbers_3char_printer.print(elem, Vec(20 + (640-40)/8 + x*(640-40)/4, 480-20 - (480-40)/8 - y*(480-40)/4), color, align = "center")
      } else if(elem < 10000) {
        numbers_4char_printer.print(elem, Vec(20 + (640 - 40) / 8 + x * (640 - 40) / 4, 480 - 20 - (480 - 40) / 8 - y * (480 - 40) / 4), color, align = "center")
      } else if(elem < 100000) {
        numbers_5char_printer.print(elem, Vec(20 + (640 - 40) / 8 + x * (640 - 40) / 4, 480 - 20 - (480 - 40) / 8 - y * (480 - 40) / 4), color, align = "center")
      } else {
        numbers_6char_printer.print(elem, Vec(20 + (640 - 40) / 8 + x * (640 - 40) / 4, 480 - 20 - (480 - 40) / 8 - y * (480 - 40) / 4), color, align = "center")
      }
    }
    if(!not_lost) print(s"You lost. Final score: $count. Play Again: F2", windowCenter, WHITE, align = "center")
    print(count, Vec(650, windowHeight-30), WHITE)
  }
}
