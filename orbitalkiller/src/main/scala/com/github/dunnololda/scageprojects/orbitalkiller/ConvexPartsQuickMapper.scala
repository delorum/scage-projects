package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec

import scala.collection.mutable.ArrayBuffer

object ConvexPartsQuickMapper extends ScageScreenAppD("ConvexPartsQuickMapper", 640, 480) {
  /*val points:List[DVec] = List(
    DVec(3.5, 2.5),
    DVec(1.5, 6.5),
    DVec(1.5, 10.5),
    DVec(-1.5, 10.5),
    DVec(-1.5, 6.5),
    DVec(-3.5, 2.5),
    DVec(-3.5, -3.5),
    DVec(3.5, -3.5)
  )*/

  // ВАЖНО: следует перечислять точки ПРОТИВ ЧАСОВОЙ СТРЕЛКИ! Если по часовой перечислять, collision detection будет плохо работать

  val points:List[DVec] = List(
    DVec(-90.0, -10.0),
    DVec(-130.0, -10.0),
    DVec(-130.0, 10.0),
    DVec(-90.0, 10.0),
    DVec(-50.0, 30.0),
    DVec(50.0, 30.0),
    DVec(90.0, 10.0),
    DVec(130.0, 10.0),
    DVec(130.0, -10.0),
    DVec(90.0, -10.0),
    DVec(50.0, -30.0),
    DVec(-50.0, -30.0)
  )

  private val cur_part = ArrayBuffer[Int]()

  private def idx2str(idx:Int):String = {
    if(idx <= 9) s"$idx"
    else {
      idx match {
        case 10 => "A"
        case 11 => "B"
        case 12 => "C"
        case 13 => "D"
        case x => s"$x"
      }
    }
  }

  key(KEY_0, onKeyDown = {Predef.print("0"); cur_part += 0})
  key(KEY_1, onKeyDown = {Predef.print("1"); cur_part += 1})
  key(KEY_2, onKeyDown = {Predef.print("2"); cur_part += 2})
  key(KEY_3, onKeyDown = {Predef.print("3"); cur_part += 3})
  key(KEY_4, onKeyDown = {Predef.print("4"); cur_part += 4})
  key(KEY_5, onKeyDown = {Predef.print("5"); cur_part += 5})
  key(KEY_6, onKeyDown = {Predef.print("6"); cur_part += 6})
  key(KEY_7, onKeyDown = {Predef.print("7"); cur_part += 7})
  key(KEY_8, onKeyDown = {Predef.print("8"); cur_part += 8})
  key(KEY_9, onKeyDown = {Predef.print("9"); cur_part += 9})
  key(KEY_A, onKeyDown = {Predef.print("A"); cur_part += 10})
  key(KEY_B, onKeyDown = {Predef.print("B"); cur_part += 11})
  key(KEY_C, onKeyDown = {Predef.print("C"); cur_part += 12})
  key(KEY_D, onKeyDown = {Predef.print("D"); cur_part += 13})

  key(KEY_SPACE, onKeyDown = {
    println()
    println(s"PolygonShape(List(${cur_part.map(x => s"DVec(${points(x).x}, ${points(x).y})").mkString(", ")}), Nil)")
    cur_part.clear()
  })

  key(KEY_Q, onKeyDown = if(keyPressed(KEY_RCONTROL) || keyPressed(KEY_LCONTROL)) stopApp())

  center = DVec.zero
  //globalScale = 20

  render {
    points.zipWithIndex.foreach {
      case (p, idx) =>
        drawFilledCircle(p, 3/globalScale, WHITE)
        print(s"  ${idx2str(idx)}", p.toVec, (max_font_size/globalScale).toFloat, WHITE)
    }
  }
}
