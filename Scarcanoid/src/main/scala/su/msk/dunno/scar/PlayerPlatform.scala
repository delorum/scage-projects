package su.msk.dunno.scar

import net.scage.support.Vec
import Scaranoid._
import net.phys2d.math.Vector2f
import org.lwjgl.opengl.GL11
import net.scage.support.physics.objects.StaticBox

object PlayerPlatform extends StaticBox(Vec(window_width/2,25), 50, 10) {
  init {
    coord = Vec(window_width/2,25)
  }

  key(KEY_LEFT,  10, onKeyDown = if(!onPause && coord.x > 60) move(Vec(-3, 0)))
  key(KEY_RIGHT, 10, onKeyDown = if(!onPause && coord.x < window_width-40) move(Vec(3, 0)))

  render {
    val verts:Array[Vector2f] = box.getPoints(body.getPosition, body.getRotation);
    currentColor = WHITE
    GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glBegin(GL11.GL_LINE_LOOP);
        verts.foreach(v => GL11.glVertex2f(v.getX, v.getY))
      GL11.glEnd();
    GL11.glEnable(GL11.GL_TEXTURE_2D);
  }
}