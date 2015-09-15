package com.github.dunnololda.scageprojects.scarcanoid

import com.github.dunnololda.scageprojects.scarcanoid.levels.LevelMap
import net.phys2d.math.Vector2f
import org.lwjgl.opengl.GL11
import com.github.dunnololda.scage.ScageLib._
import Scaranoid._

object Level {
  private var boxes:List[Physical] = Nil
  def winCondition = boxes.forall(!physics.containsPhysical(_))

  def loadMap(level_map:LevelMap) {
    physics.removePhysicals(boxes:_*)
    boxes = level_map.load
  }

  physics.addPhysicals(
    new LevelEdge(Vec(30,              10),              Vec(30,             windowHeight-10)),
    new LevelEdge(Vec(30,              windowHeight-10), Vec(windowWidth-10, windowHeight-10)),
    new LevelEdge(Vec(windowWidth-10,  windowHeight-10), Vec(windowWidth-10, 10)),
    new LevelEdge(Vec(windowWidth-10,  10),              Vec(30,             10)) {
      /*init {
        clearTouches()
      }*/

      action {
        if(isTouching(PlayerBall)) pause()
      }
    }
  )

  val additional_platform = physics.addPhysical(new StaticBox(Vec(windowWidth/4, 200), 150, 10) {
    init {
      coord = Vec(windowWidth/4, 200)
    }

    private var dir = 1
    action {
      if(isTouching(PlayerBall)) PlayerBall.ball_color = WHITE
      move(Vec(dir,0))
      if(coord.x > windowWidth-90) dir = -1
      else if(coord.x < 110) dir = 1
    }

    render {
      val verts:Array[Vector2f] = box.getPoints(body.getPosition, body.getRotation)
      currentColor = WHITE
      GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_LINE_LOOP)
      verts.foreach(v => GL11.glVertex2f(v.getX, v.getY))
        GL11.glEnd()
      GL11.glEnable(GL11.GL_TEXTURE_2D)
    }
  })
}