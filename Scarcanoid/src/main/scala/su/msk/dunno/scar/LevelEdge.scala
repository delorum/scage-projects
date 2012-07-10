package su.msk.dunno.scar

import net.scage.support.Vec
import Scaranoid._
import net.phys2d.math.Vector2f
import net.scage.support.physics.objects.StaticLine

class LevelEdge (from:Vec, to:Vec) extends StaticLine(from, to) {
  render {
    val verts:Array[Vector2f] = line.getVertices(body.getPosition(), body.getRotation());
    currentColor = WHITE
    drawLine(Vec(verts(0).getX, verts(0).getY),
             Vec(verts(1).getX, verts(1).getY))
  }
}