package com.github.dunnololda.scageprojects.rubic

import com.github.dunnololda.scage.ScageLib._

class RubicCube(side1_center:Vec, elem_size:Int) {
  private val side2_center = side1_center + Vec(0, elem_size*3)
  private val side3_center = side1_center + Vec(0, elem_size*6)
  private val side4_center = side1_center + Vec(0, elem_size*9)
  private val side5_center = side1_center + Vec(-elem_size*3, elem_size*3)
  private val side6_center = side1_center + Vec(elem_size*3, elem_size*3)

  private var side1 = Array.ofDim[RubicElement](3, 3)
  private var side2 = Array.ofDim[RubicElement](3, 3)
  private var side3 = Array.ofDim[RubicElement](3, 3)
  private var side4 = Array.ofDim[RubicElement](3, 3)
  private var side5 = Array.ofDim[RubicElement](3, 3)
  private var side6 = Array.ofDim[RubicElement](3, 3)

  def reset() {
    for {
      i <- 0 to 2
      j <- 0 to 2
    } {
      side1(i)(j) = RubicElement(RED, i*3 + j)
      side2(i)(j) = RubicElement(BLUE, i*3 + j)
      side3(i)(j) = RubicElement(ORANGE, i*3 + j)
      side4(i)(j) = RubicElement(GREEN, i*3 + j)
      side5(i)(j) = RubicElement(YELLOW, i*3 + j)
      side6(i)(j) = RubicElement(DARK_GRAY, i*3 + j)
    }
  }
  reset()

  private def drawSide(side:Array[Array[RubicElement]], side_center:Vec) {
    val upper_left = side_center + Vec(-elem_size*1.5f, elem_size*1.5f)
    for {
      i <- 0 to 3
      j <- 0 to 3
    } {
      drawLine(Vec(upper_left.x, upper_left.y - elem_size*i), Vec(upper_left.x + elem_size*3, upper_left.y - elem_size*i), WHITE)
      drawLine(Vec(upper_left.x + elem_size*j, upper_left.y), Vec(upper_left.x + elem_size*j, upper_left.y - elem_size*3), WHITE)
    }
    val upper_left_elem = side_center + Vec(-elem_size, elem_size)
    for {
      i <- 0 to 2
      j <- 0 to 2
    } {
      drawFilledRectCentered(
        Vec(upper_left_elem.x + elem_size*i, upper_left_elem.y - elem_size*j),
        elem_size-1, elem_size-1,
        side(j)(i).color
      )
      print(side(j)(i).position, Vec(upper_left_elem.x + elem_size*i, upper_left_elem.y - elem_size*j), BLACK, align = "center")
    }
  }

  def draw() {
    drawSide(side1, side1_center)
    drawSide(side2, side2_center)
    drawSide(side3, side3_center)
    drawSide(side4, side4_center)
    drawSide(side5, side5_center)
    drawSide(side6, side6_center)

    print("Q", side1_center + Vec(-elem_size, -elem_size*2), WHITE, align = "center")
    print("A", side1_center + Vec(-elem_size, -elem_size*3), WHITE, align = "center")
    print("W", side1_center + Vec(0, -elem_size*2), WHITE, align = "center")
    print("S", side1_center + Vec(0, -elem_size*3), WHITE, align = "center")
    print("E", side1_center + Vec(elem_size, -elem_size*2), WHITE, align = "center")
    print("D", side1_center + Vec(elem_size, -elem_size*3), WHITE, align = "center")

    print("R", side5_center + Vec(-elem_size*2, elem_size), WHITE, align = "center")
    print("F", side5_center + Vec(-elem_size*3, elem_size), WHITE, align = "center")
    print("T", side5_center + Vec(-elem_size*2, 0), WHITE, align = "center")
    print("G", side5_center + Vec(-elem_size*3, 0), WHITE, align = "center")
    print("Y", side5_center + Vec(-elem_size*2, -elem_size), WHITE, align = "center")
    print("H", side5_center + Vec(-elem_size*3, -elem_size), WHITE, align = "center")

    print("U", side5_center + Vec(-elem_size, elem_size*2), WHITE, align = "center")
    print("J", side5_center + Vec(-elem_size, elem_size*3), WHITE, align = "center")
    print("I", side5_center + Vec(0, elem_size*2), WHITE, align = "center")
    print("K", side5_center + Vec(0, elem_size*3), WHITE, align = "center")
    print("O", side5_center + Vec(elem_size, elem_size*2), WHITE, align = "center")
    print("L", side5_center + Vec(elem_size, elem_size*3), WHITE, align = "center")
  }

  private case class SideElem(i:Int, j:Int)
  private case class SideLine(side:Array[Array[RubicElement]], elem1:SideElem, elem2:SideElem, elem3:SideElem)

  private def moveLine(line1: SideLine, line2:SideLine, line3:SideLine, line4:SideLine) {
    val tmp1 = line1.side(line1.elem1.i)(line1.elem1.j)
    val tmp2 = line1.side(line1.elem2.i)(line1.elem2.j)
    val tmp3 = line1.side(line1.elem3.i)(line1.elem3.j)

    line1.side(line1.elem1.i)(line1.elem1.j) = line4.side(line4.elem1.i)(line4.elem1.j)
    line1.side(line1.elem2.i)(line1.elem2.j) = line4.side(line4.elem2.i)(line4.elem2.j)
    line1.side(line1.elem3.i)(line1.elem3.j) = line4.side(line4.elem3.i)(line4.elem3.j)

    line4.side(line4.elem1.i)(line4.elem1.j) = line3.side(line3.elem1.i)(line3.elem1.j)
    line4.side(line4.elem2.i)(line4.elem2.j) = line3.side(line3.elem2.i)(line3.elem2.j)
    line4.side(line4.elem3.i)(line4.elem3.j) = line3.side(line3.elem3.i)(line3.elem3.j)

    line3.side(line3.elem1.i)(line3.elem1.j) = line2.side(line2.elem1.i)(line2.elem1.j)
    line3.side(line3.elem2.i)(line3.elem2.j) = line2.side(line2.elem2.i)(line2.elem2.j)
    line3.side(line3.elem3.i)(line3.elem3.j) = line2.side(line2.elem3.i)(line2.elem3.j)

    line2.side(line2.elem1.i)(line2.elem1.j) = tmp1
    line2.side(line2.elem2.i)(line2.elem2.j) = tmp2
    line2.side(line2.elem3.i)(line2.elem3.j) = tmp3
  }

  private def rotatePlane(side:Array[Array[RubicElement]], clockwise:Boolean) {
    if(clockwise) {
      val tmp1 = side(0)(0)
      side(0)(0) = side(2)(0)
      side(2)(0) = side(2)(2)
      side(2)(2) = side(0)(2)
      side(0)(2) = tmp1

      val tmp2 = side(0)(1)
      side(0)(1) = side(1)(0)
      side(1)(0) = side(2)(1)
      side(2)(1) = side(1)(2)
      side(1)(2) = tmp2
    } else {
      val tmp1 = side(0)(0)
      side(0)(0) = side(0)(2)
      side(0)(2) = side(2)(2)
      side(2)(2) = side(2)(0)
      side(2)(0) = tmp1

      val tmp2 = side(0)(1)
      side(0)(1) = side(1)(2)
      side(1)(2) = side(2)(1)
      side(2)(1) = side(1)(0)
      side(1)(0) = tmp2
    }
  }

  def rotateRightAroundSide6() {
    val tmp = side1
    side1 = side4
    side4 = side3
    side3 = side2
    side2 = tmp

    rotatePlane(side5, clockwise = false)
    rotatePlane(side6, clockwise = true)
  }

  def rotateLeftAroundSide6() {
    val tmp = side1
    side1 = side2
    side2 = side3
    side3 = side4
    side4 = tmp

    rotatePlane(side5, clockwise = true)
    rotatePlane(side6, clockwise = false)
  }

  def rotateRightAroundSide1() {
    rotatePlane(side4, clockwise = true)
    rotatePlane(side4, clockwise = true)

    rotatePlane(side6, clockwise = true)
    rotatePlane(side6, clockwise = true)

    val tmp = side2
    side2 = side5
    side5 = side4
    side4 = side6
    side6 = tmp

    rotatePlane(side1, clockwise = true)
    rotatePlane(side3, clockwise = false)
  }

  def rotateLeftAroundSide1() {
    rotatePlane(side4, clockwise = true)
    rotatePlane(side4, clockwise = true)

    rotatePlane(side5, clockwise = true)
    rotatePlane(side5, clockwise = true)

    val tmp = side2
    side2 = side6
    side6 = side4
    side4 = side5
    side5 = tmp

    rotatePlane(side1, clockwise = false)
    rotatePlane(side3, clockwise = true)
  }

  def rotateRightAroundSide2() {
    rotatePlane(side2, clockwise = true)
    rotatePlane(side4, clockwise = false)

    rotatePlane(side5, clockwise = true)
    rotatePlane(side3, clockwise = true)
    rotatePlane(side6, clockwise = true)
    rotatePlane(side1, clockwise = true)

    val tmp = side5
    side5 = side1
    side1 = side6
    side6 = side3
    side3 = tmp
  }

  def rotateLeftAroundSide2() {
    rotatePlane(side2, clockwise = false)
    rotatePlane(side4, clockwise = true)

    rotatePlane(side5, clockwise = false)
    rotatePlane(side3, clockwise = false)
    rotatePlane(side6, clockwise = false)
    rotatePlane(side1, clockwise = false)

    val tmp = side5
    side5 = side3
    side3 = side6
    side6 = side1
    side1 = tmp
  }

  def command(command:Char) {
    command match {
      case 'q' | 'Q' => q()
      case 'a' | 'A' => a()
      case 'w' | 'W' => w()
      case 's' | 'S' => s()
      case 'e' | 'E' => e()
      case 'd' | 'D' => d()
      case 'r' | 'R' => r()
      case 'f' | 'F' => f()
      case 't' | 'T' => t()
      case 'g' | 'G' => g()
      case 'y' | 'Y' => y()
      case 'h' | 'H' => h()
      case 'u' | 'U' => u()
      case 'j' | 'J' => j()
      case 'i' | 'I' => i()
      case 'k' | 'K' => k()
      case 'o' | 'O' => o()
      case 'l' | 'L' => l()
    }
  }

  // ==============================================================

  def q() {
    moveLine(
      SideLine(side1, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side2, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side3, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side4, SideElem(2,0), SideElem(1,0), SideElem(0,0))
    )

    rotatePlane(side5, clockwise = false)
  }

  def a() {
    moveLine(
      SideLine(side1, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side4, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side3, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side2, SideElem(2,0), SideElem(1,0), SideElem(0,0))
    )

    rotatePlane(side5, clockwise = true)
  }

  // ==============================================================

  def w() {
    moveLine(
      SideLine(side1, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side2, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side3, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side4, SideElem(2,1), SideElem(1,1), SideElem(0,1))
    )
  }

  def s() {
    moveLine(
      SideLine(side1, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side4, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side3, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side2, SideElem(2,1), SideElem(1,1), SideElem(0,1))
    )
  }

  // ==============================================================

  def e() {
    moveLine(
      SideLine(side1, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side2, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side3, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side4, SideElem(2,2), SideElem(1,2), SideElem(0,2))
    )

    rotatePlane(side6, clockwise = true)
  }

  def d() {
    moveLine(
      SideLine(side1, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side4, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side3, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side2, SideElem(2,2), SideElem(1,2), SideElem(0,2))
    )

    rotatePlane(side6, clockwise = false)
  }

  // ==============================================================

  def r() {
    moveLine(
      SideLine(side5, SideElem(0,0), SideElem(0,1), SideElem(0,2)),
      SideLine(side2, SideElem(0,0), SideElem(0,1), SideElem(0,2)),
      SideLine(side6, SideElem(0,0), SideElem(0,1), SideElem(0,2)),
      SideLine(side4, SideElem(2,2), SideElem(2,1), SideElem(2,0))
    )

    rotatePlane(side3, clockwise = false)
  }

  def f() {
    moveLine(
      SideLine(side5, SideElem(0,0), SideElem(0,1), SideElem(0,2)),
      SideLine(side4, SideElem(2,2), SideElem(2,1), SideElem(2,0)),
      SideLine(side6, SideElem(0,0), SideElem(0,1), SideElem(0,2)),
      SideLine(side2, SideElem(0,0), SideElem(0,1), SideElem(0,2))
    )

    rotatePlane(side3, clockwise = true)
  }

  // ==============================================================

  def t() {
    moveLine(
      SideLine(side5, SideElem(1,0), SideElem(1,1), SideElem(1,2)),
      SideLine(side2, SideElem(1,0), SideElem(1,1), SideElem(1,2)),
      SideLine(side6, SideElem(1,0), SideElem(1,1), SideElem(1,2)),
      SideLine(side4, SideElem(1,2), SideElem(1,1), SideElem(1,0))
    )
  }

  def g() {
    moveLine(
      SideLine(side5, SideElem(1,0), SideElem(1,1), SideElem(1,2)),
      SideLine(side4, SideElem(1,2), SideElem(1,1), SideElem(1,0)),
      SideLine(side6, SideElem(1,0), SideElem(1,1), SideElem(1,2)),
      SideLine(side2, SideElem(1,0), SideElem(1,1), SideElem(1,2))
    )
  }

  // ==============================================================

  def y() {
    moveLine(
      SideLine(side5, SideElem(2,0), SideElem(2,1), SideElem(2,2)),
      SideLine(side2, SideElem(2,0), SideElem(2,1), SideElem(2,2)),
      SideLine(side6, SideElem(2,0), SideElem(2,1), SideElem(2,2)),
      SideLine(side4, SideElem(0,2), SideElem(0,1), SideElem(0,0))
    )

    rotatePlane(side1, clockwise = true)
  }

  def h() {
    moveLine(
      SideLine(side5, SideElem(2,0), SideElem(2,1), SideElem(2,2)),
      SideLine(side4, SideElem(0,2), SideElem(0,1), SideElem(0,0)),
      SideLine(side6, SideElem(2,0), SideElem(2,1), SideElem(2,2)),
      SideLine(side2, SideElem(2,0), SideElem(2,1), SideElem(2,2))
    )

    rotatePlane(side1, clockwise = false)
  }

  // ==============================================================

  def u() {
    moveLine(
      SideLine(side5, SideElem(0,0), SideElem(1,0), SideElem(2,0)),
      SideLine(side1, SideElem(2,0), SideElem(2,1), SideElem(2,2)),
      SideLine(side6, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side3, SideElem(0,2), SideElem(0,1), SideElem(0,0))
    )

    rotatePlane(side4, clockwise = false)
  }

  def j() {
    moveLine(
      SideLine(side5, SideElem(0,0), SideElem(1,0), SideElem(2,0)),
      SideLine(side3, SideElem(0,2), SideElem(0,1), SideElem(0,0)),
      SideLine(side6, SideElem(2,2), SideElem(1,2), SideElem(0,2)),
      SideLine(side1, SideElem(2,0), SideElem(2,1), SideElem(2,2))
    )

    rotatePlane(side4, clockwise = true)
  }

  // ==============================================================

  def i() {
    moveLine(
      SideLine(side5, SideElem(0,1), SideElem(1,1), SideElem(2,1)),
      SideLine(side1, SideElem(1,0), SideElem(1,1), SideElem(1,2)),
      SideLine(side6, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side3, SideElem(1,2), SideElem(1,1), SideElem(1,0))
    )
  }

  def k() {
    moveLine(
      SideLine(side5, SideElem(0,1), SideElem(1,1), SideElem(2,1)),
      SideLine(side3, SideElem(1,2), SideElem(1,1), SideElem(1,0)),
      SideLine(side6, SideElem(2,1), SideElem(1,1), SideElem(0,1)),
      SideLine(side1, SideElem(1,0), SideElem(1,1), SideElem(1,2))
    )
  }

  // ==============================================================

  def o() {
    moveLine(
      SideLine(side5, SideElem(0,2), SideElem(1,2), SideElem(2,2)),
      SideLine(side1, SideElem(0,0), SideElem(0,1), SideElem(0,2)),
      SideLine(side6, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side3, SideElem(2,2), SideElem(2,1), SideElem(2,0))
    )
    rotatePlane(side2, clockwise = false)
  }

  def l() {
    moveLine(
      SideLine(side5, SideElem(0,2), SideElem(1,2), SideElem(2,2)),
      SideLine(side3, SideElem(2,2), SideElem(2,1), SideElem(2,0)),
      SideLine(side6, SideElem(2,0), SideElem(1,0), SideElem(0,0)),
      SideLine(side1, SideElem(0,0), SideElem(0,1), SideElem(0,2))
    )
    rotatePlane(side2, clockwise = true)
  }
}
