package com.github.dunnololda.scageprojects.scarcanoid

import net.phys2d.math.Vector2f
import org.lwjgl.opengl.GL11
import com.github.dunnololda.scage.ScageLib._
import Scaranoid._

object PlayerPlatform extends StaticBox(Vec(windowWidth/2,25), 50, 10) {
  init {
    coord = Vec(windowWidth/2,25)
  }

  key(KEY_LEFT,  10, onKeyDown = if(!onPause && coord.x > 60) move(Vec(-3, 0)))
  key(KEY_RIGHT, 10, onKeyDown = if(!onPause && coord.x < windowWidth-40) move(Vec(3, 0)))

  render {
    val verts:Array[Vector2f] = box.getPoints(body.getPosition, body.getRotation)
    currentColor = WHITE
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glBegin(GL11.GL_LINE_LOOP)
    verts.foreach(v => GL11.glVertex2f(v.getX, v.getY))
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }
}