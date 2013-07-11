package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import org.lwjgl.opengl.GL11

object RotationTests extends ScageScreenApp("Rotation Test", 640 ,480) {
  var _rotation = 0f
  action(50) {
    _rotation = (_rotation+0.05f) % (2*math.Pi.toFloat)
  }
  val c = Vec(320, 240)
  render {
    openglLocalTransform {
      openglMove(c-Vec(40,40))

      openglMove(Vec(40, 40))
      openglRotateRad(-_rotation)
      openglMove(Vec(-40, -40))

      GL11.glPushMatrix()
      /*openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))*/

      openglMove(Vec(40, 40))
      openglRotateRad(_rotation)
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()

      //openglRotateRad(-rotation)

      GL11.glPushMatrix()
      /*openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))*/

      openglMove(Vec(40, -40))
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()

      GL11.glPushMatrix()
      /*openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))*/

      openglMove(Vec(-40, -40))
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()

      GL11.glPushMatrix()
      /*openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))*/

      openglMove(Vec(-40, 40))
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()
    }
  }
}

