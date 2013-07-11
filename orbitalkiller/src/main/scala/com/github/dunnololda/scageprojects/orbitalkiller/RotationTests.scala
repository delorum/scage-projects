package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import org.lwjgl.opengl.GL11

object RotationTests extends ScageScreenApp("Rotation Test", 640 ,480) {
  var rotation = 0f
  action(50) {
    rotation = (rotation+0.05f) % (2*math.Pi.toFloat)
  }
  val c = Vec(320, 240)
  render {
    openglLocalTransform {
      //openglMove(Vec(40,40))
      //openglMove(c-Vec(40,40))
      //openglRotateRad(-rotation)
      /*openglMove((c-Vec(40,40))*(-1))*/
      openglMove(c-Vec(40,40))

      GL11.glPushMatrix()
      openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))

      openglMove(Vec(40, 40))
      openglRotateRad(rotation)
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()

      //openglRotateRad(-rotation)

      GL11.glPushMatrix()
      openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))

      openglMove(Vec(40, -40))
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()

      GL11.glPushMatrix()
      openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))

      openglMove(Vec(-40, -40))
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()

      GL11.glPushMatrix()
      openglMove(Vec(40, 40))
      openglRotateRad(-rotation)
      openglMove(Vec(-40, -40))

      openglMove(Vec(-40, 40))
      drawRectCentered(Vec.zero, 20, 20)
      GL11.glPopMatrix()
    }
  }
}

