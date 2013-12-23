package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

case class Rect(position:Vec) {
  val points = List(
    position+Vec(10, -10),
    position+Vec(10, 10),
    position+Vec(-10, 10),
    position+Vec(-10, -10)
  )
}

object ShapeTests extends ScageScreenApp("Shape Tests", 640, 480) {
  private val rects = mutable.HashSet[Rect]()

  key(KEY_C, onKeyDown = rects.clear())

  private def isValid(pos:Vec, rects:mutable.HashSet[Rect]):Boolean = {
    rects.isEmpty ||
    rects.contains(Rect(pos+Vec(0, 20))) ||
    rects.contains(Rect(pos+Vec(0, -20))) ||
    rects.contains(Rect(pos+Vec(20, 0))) ||
    rects.contains(Rect(pos+Vec(-20, 0)))
  }

  leftMouse(onBtnDown = m => {
    val pos = Vec(m.ix/20*20+10, m.iy/20*20+10)
    if(isValid(pos, rects)) {
      rects += Rect(pos)
    }
  })

  rightMouse(onBtnDown = m => {
    val pos = Vec(m.ix/20*20+10, m.iy/20*20+10)
    val without = rects - Rect(pos)
    if(without.forall(p => isValid(p.position, without - p))) {
      rects -= Rect(pos)
    }
  })

  render {
    (0 to windowWidth by 20).foreach(x => drawLine(Vec(x, 0), Vec(x, windowHeight), DARK_GRAY))
    (0 to windowHeight by 20).foreach(y => drawLine(Vec(0, y), Vec(windowWidth, y), DARK_GRAY))
  }

  private def rightDownVertice:Option[Vec] = {
    if(rects.isEmpty) None
    else {
      val all_points = rects.flatMap(_.points).toList
      val max_x = all_points.sortBy(-_.x).head.x
      val right_points = all_points.filter(_.x == max_x)
      Some(right_points.sortBy(_.y).head)
    }
  }

  private def outerPoints:List[Vec] = {
    if(rects.isEmpty) Nil
    else {
      val all_points = rects.flatMap(_.points)
      val outer = all_points.filter {
        case p =>
          val up    = p + Vec(0, 20)
          val down  = p + Vec(0, -20)
          val right = p + Vec(20, 0)
          val left  = p + Vec(-20, 0)
          List(up, down, right, left).exists(r => !all_points.contains(r))
      }.toList
      val center = Vec(outer.map(_.x).sum/outer.length, outer.map(_.y).sum/outer.length)
      val outer1 = outer.map(p => (p, (p - center).deg(Vec(1, 0))*((p-center)*Vec(0, 1)).signum)).sortBy(_._2)
      val outer2 = outer1.map(_._1)
      outer2 ::: outer2.head :: Nil
    }
  }

  render {
    rects.foreach {
      case Rect(position) => drawFilledRectCentered(position, 18, 18, GRAY)
    }

    rightDownVertice.foreach(p => drawFilledCircle(p, 3, GREEN))
    //outerPoints.foreach(p => drawFilledCircle(p, 3, RED))
    drawSlidingLines(outerPoints, RED)
  }
}
