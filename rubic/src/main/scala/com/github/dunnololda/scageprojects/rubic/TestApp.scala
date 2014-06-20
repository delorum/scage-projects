package com.github.dunnololda.scageprojects.rubic

import com.github.dunnololda.scage.ScageLib._

sealed trait RubicElementColor
case object RubicRed extends RubicElementColor
case object RubicBlue extends RubicElementColor
case object RubicOrange extends RubicElementColor
case object RubicGreen extends RubicElementColor
case object RubicWhite extends RubicElementColor
case object RubicYellow extends RubicElementColor

case class RubicElement(color:ScageColor, position:Int)

class RubicCube(val side1_center:Vec, elem_size:Int) {
  val side2_center = side1_center + Vec(0, elem_size*3)
  val side3_center = side1_center + Vec(0, elem_size*6)
  val side4_center = side1_center + Vec(0, elem_size*9)
  val side5_center = side1_center + Vec(-elem_size*3, elem_size*3)
  val side6_center = side1_center + Vec(elem_size*3, elem_size*3)

  val side1 = Array.ofDim[RubicElement](3, 3)
  val side2 = Array.ofDim[RubicElement](3, 3)
  val side3 = Array.ofDim[RubicElement](3, 3)
  val side4 = Array.ofDim[RubicElement](3, 3)
  val side5 = Array.ofDim[RubicElement](3, 3)
  val side6 = Array.ofDim[RubicElement](3, 3)

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

object TestApp extends ScageScreenApp("Test", 800, 600) {
  val rubic = new RubicCube(center + Vec(0, -30*4), 30)

  keyIgnorePause(KEY_Q, onKeyDown = rubic.q())
  keyIgnorePause(KEY_A, onKeyDown = rubic.a())
  keyIgnorePause(KEY_W, onKeyDown = rubic.w())
  keyIgnorePause(KEY_S, onKeyDown = rubic.s())
  keyIgnorePause(KEY_E, onKeyDown = rubic.e())
  keyIgnorePause(KEY_D, onKeyDown = rubic.d())
  keyIgnorePause(KEY_R, onKeyDown = rubic.r())
  keyIgnorePause(KEY_F, onKeyDown = rubic.f())
  keyIgnorePause(KEY_T, onKeyDown = rubic.t())
  keyIgnorePause(KEY_G, onKeyDown = rubic.g())
  keyIgnorePause(KEY_Y, onKeyDown = rubic.y())
  keyIgnorePause(KEY_H, onKeyDown = rubic.h())
  keyIgnorePause(KEY_U, onKeyDown = rubic.u())
  keyIgnorePause(KEY_J, onKeyDown = rubic.j())
  keyIgnorePause(KEY_I, onKeyDown = rubic.i())
  keyIgnorePause(KEY_K, onKeyDown = rubic.k())
  keyIgnorePause(KEY_O, onKeyDown = rubic.o())
  keyIgnorePause(KEY_L, onKeyDown = rubic.l())

  keyIgnorePause(KEY_SPACE, onKeyDown = rubic.reset())

  render {
    rubic.draw()
  }

}
