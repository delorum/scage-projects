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

object Rubic extends ScageScreenApp("Rubic Cube", 1024, 768) {
  private val rubic = new RubicCube(center + Vec(0, -30*4), 30)
  private val commands = collection.mutable.ArrayBuffer[Char]()
  private var shift_pressed = false

  keyIgnorePause(KEY_Q, onKeyDown = {Predef.print("Q"); rubic.q()})
  keyIgnorePause(KEY_A, onKeyDown = {Predef.print("A"); rubic.a()})
  keyIgnorePause(KEY_W, onKeyDown = {Predef.print("W"); rubic.w()})
  keyIgnorePause(KEY_S, onKeyDown = {Predef.print("S"); rubic.s()})
  keyIgnorePause(KEY_E, onKeyDown = {Predef.print("E"); rubic.e()})
  keyIgnorePause(KEY_D, onKeyDown = {Predef.print("D"); rubic.d()})
  keyIgnorePause(KEY_R, onKeyDown = {Predef.print("R"); rubic.r()})
  keyIgnorePause(KEY_F, onKeyDown = {Predef.print("F"); rubic.f()})
  keyIgnorePause(KEY_T, onKeyDown = {Predef.print("T"); rubic.t()})
  keyIgnorePause(KEY_G, onKeyDown = {Predef.print("G"); rubic.g()})
  keyIgnorePause(KEY_Y, onKeyDown = {Predef.print("Y"); rubic.y()})
  keyIgnorePause(KEY_H, onKeyDown = {Predef.print("H"); rubic.h()})
  keyIgnorePause(KEY_U, onKeyDown = {Predef.print("U"); rubic.u()})
  keyIgnorePause(KEY_J, onKeyDown = {Predef.print("J"); rubic.j()})
  keyIgnorePause(KEY_I, onKeyDown = {Predef.print("I"); rubic.i()})
  keyIgnorePause(KEY_K, onKeyDown = {Predef.print("K"); rubic.k()})
  keyIgnorePause(KEY_O, onKeyDown = {Predef.print("O"); rubic.o()})
  keyIgnorePause(KEY_L, onKeyDown = {Predef.print("L"); rubic.l()})

  keyIgnorePause(KEY_SPACE, onKeyDown = {Predef.println(); rubic.reset()})

  keyIgnorePause(KEY_LSHIFT, onKeyDown = shift_pressed = true, onKeyUp = shift_pressed = false)
  keyIgnorePause(KEY_RSHIFT, onKeyDown = shift_pressed = true, onKeyUp = shift_pressed = false)

  keyIgnorePause(KEY_UP, onKeyDown = rubic.rotateRightAroundSide6())
  keyIgnorePause(KEY_DOWN, onKeyDown = rubic.rotateLeftAroundSide6())
  keyIgnorePause(KEY_RIGHT, onKeyDown = {
    if(shift_pressed) rubic.rotateRightAroundSide2()
    else rubic.rotateRightAroundSide1()
  })
  keyIgnorePause(KEY_LEFT, onKeyDown = {
    if(shift_pressed) rubic.rotateLeftAroundSide2()
    else rubic.rotateLeftAroundSide1()
  })

  keyIgnorePause(KEY_1, onKeyDown = commands ++= "WLWLWLWWLWLWLWLL")
  keyIgnorePause(KEY_2, onKeyDown = commands ++= "WLWLWLWOWLWLWLWO")
  keyIgnorePause(KEY_3, onKeyDown = commands ++= "YYLWLLSLYY")
  keyIgnorePause(KEY_4, onKeyDown = commands ++= "YYOWLLSOYY")
  keyIgnorePause(KEY_5, onKeyDown = commands ++= "WWLLWWLL")
  keyIgnorePause(KEY_6, onKeyDown = commands ++= "EELLQQJJEELLQQJJEELLQQJJ")
  keyIgnorePause(KEY_7, onKeyDown = commands ++= "EHDYEHDYLEHDYEHDYEHDYEHDYO")
  keyIgnorePause(KEY_8, onKeyDown = commands ++= "EHDYEHDYOEHDYEHDYEHDYEHDYL")
  keyIgnorePause(KEY_9, onKeyDown = commands ++= "EIEIEIEIOEIEIEIEIL")
  keyIgnorePause(KEY_0, onKeyDown = commands ++= "EIEIEIEILEIEIEIEIO")

  actionStaticPeriodIgnorePause(100) {
    if(commands.nonEmpty) {
      val command = commands.remove(0)
      rubic.command(command)
    }
  }

  render {
    rubic.draw()
  }
}
