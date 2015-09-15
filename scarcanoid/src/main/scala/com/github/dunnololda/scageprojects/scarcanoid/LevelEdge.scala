package com.github.dunnololda.scageprojects.scarcanoid

import net.phys2d.math.Vector2f
import com.github.dunnololda.scage.ScageLib._
import Scaranoid._

class LevelEdge (from:Vec, to:Vec) extends StaticLine(from, to) {
  render {
    currentColor = WHITE
    drawLine(Vec(points(0).x, points(0).y),
             Vec(points(1).x, points(1).y))
  }
}