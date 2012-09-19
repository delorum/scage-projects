package su.msk.dunno.scage.tutorials.gravitation

import net.scage.support.Vec
import org.lwjgl.opengl.GL11
import net.scage.ScageLib._
import Orbita._

class SpaceShip(init_velocity:Vec)
extends MaterialPoint(init_velocity = init_velocity, mass = property("ship.mass", 0.1f), radius = 1) {
  //private val SHIP = image("plane2.png", 3, 3, 0, 0, 122, 121)
  render {
    currentColor = GREEN
    GL11.glPushMatrix()
      GL11.glTranslatef(location.x, location.y, 0.0f)
      GL11.glRotatef(_rotation, 0.0f, 0.0f, 1.0f)
      //GL11.glCallList(SHIP)
      drawFilledPolygon(Vec(-1,-1), Vec(1,-1), Vec(0,2))
    GL11.glPopMatrix()

    /*GL11.glPushMatrix();
      GL11.glTranslatef(coord.x, coord.y, 0.0f)
      GL11.glScalef(1.5f/scale, 1.5f/scale, 1)
      print(mass, 0, 0, YELLOW)
      print(coord, 0, -15, YELLOW)
      print(_velocity, 0, -30, YELLOW)
    GL11.glPopMatrix()*/

    drawLine(location, location+_direction*20, RED)
    drawLine(location, location+_velocity, CYAN)

    flight_plan.foreach {
      case (pos, _, _, direction, _, _) =>
        drawFilledCircle(pos, 3f/globalScale, CYAN)
        drawLine(pos, pos+direction*10, CYAN)
    }
  }
}