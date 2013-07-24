package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

sealed trait Shape {
  def collisions(other_shape:Shape):List[Vec]
}

case class CircleShape(center:Vec, radius:Float) extends Shape {
  def collisions(other_shape: Shape): List[Vec] = {
    other_shape match {
      case CircleShape(Vec(x2, y2), r2) =>
        val Vec(x1, y1) = center
        val r1 = radius
        val d = math.sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)).toFloat
        if(d < r1+r2 && d >= math.abs(r1-r2)) {
          val b = (r2*r2 - r1*r1)/(2*d)
          val a = d - b
          val h = math.sqrt(r2*r2 - b*b).toFloat

          val x = x1 + (x2-x1)/(d/a)
          val y = x1 + (x2-x1)/(d/a)

          val x3 = x - (y-y2)*h/b
          val y3 = y + (x-x2)*h/b
          val x4 = x+(y-y2)*h/b
          val y4 = y-(x-x2)*h/b

          List(Vec(x3, y3), Vec(x4, y4))
        } else if(d == r1+r2) {

          val b = (r2*r2 - r1*r1)/(2*d)
          val a = d - b

          val x = x1 + (x2-x1)/(d/a)
          val y = x1 + (x2-x1)/(d/a)

          List(Vec(x, y))
        } else Nil
      case _ => Nil
    }
  }
}

object CollisionTests extends ScageScreenApp("Collision Tests", 800, 600){

}


