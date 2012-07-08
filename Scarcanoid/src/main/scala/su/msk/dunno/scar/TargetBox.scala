package su.msk.dunno.scar

import net.scage.support.{ScageColor, Vec}
import net.phys2d.math.Vector2f
import org.lwjgl.opengl.GL11
import Scaranoid._
import net.scage.support.physics.objects.StaticBox

class TargetBox(leftup_coord:Vec) extends StaticBox(leftup_coord, 40, 40) {
  val box_color = {
    (math.random*3).toInt match {
      case 0 => RED
      case 1 => YELLOW
      case 2 => BLUE
    }
  }

  implicit def compareColors(color:ScageColor) = new ScalaObject {
    def >(other_color:ScageColor) = {
      color match {
        case RED => other_color == YELLOW
        case YELLOW => other_color == BLUE
        case BLUE => other_color == RED
        case _ => false
      }
    }
  }

  def colorModificator(box_color:ScageColor) = {
    if(PlayerBall.ball_color == WHITE) 4
    if(PlayerBall.ball_color > box_color) 3
    else if(PlayerBall.ball_color == box_color) 2
    else 1
  }

  def isActive = physics.containsPhysical(this)

  action {
    if(isActive) {
      if(isTouching(PlayerBall)) {
        Scaranoid.count += Scaranoid.bonus
        Scaranoid.bonus = colorModificator(box_color)

        PlayerBall.ball_color = box_color
        physics.removePhysicals(this)

        if(Level.winCondition) pause()
        delActions(currentOperation)
      }
    } else delActions(currentOperation)
  }

  render {
    if(isActive) {
      /*color = box_color
      val verts:Array[Vector2f] = box.getPoints(body.getPosition(), body.getRotation());
      GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glBegin(GL11.GL_QUADS);
          verts.foreach(v => GL11.glVertex2f(v.getX, v.getY))
        GL11.glEnd();
      GL11.glEnable(GL11.GL_TEXTURE_2D);*/
      drawFilledRectCentered(coord, box_width, box_height, box_color)
    } else delRenders(currentOperation)
  }
}