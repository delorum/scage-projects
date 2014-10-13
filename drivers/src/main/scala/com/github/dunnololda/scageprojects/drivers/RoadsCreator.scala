package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

object RoadsCreator extends ScageScreenApp("Roads Creator", 800, 600) {
  private var _center = windowCenter
  private var _road_start:Option[Vec] = None

  private val roads = ArrayBuffer[RoadElement]()
  private var selected_road_element = -1

  key(KEY_W, 100, onKeyDown = _center += Vec(0, 5))
  key(KEY_A, 100, onKeyDown = _center -= Vec(5, 0))
  key(KEY_S, 100, onKeyDown = _center -= Vec(0, 5))
  key(KEY_D, 100, onKeyDown = _center += Vec(5, 0))

  key(KEY_LEFT, onKeyDown = {
    if(roads.nonEmpty) {
      selected_road_element += 1
      if(selected_road_element >= roads.length) selected_road_element = -1
    } else selected_road_element = -1
  })
  key(KEY_RIGHT, onKeyDown = {
    if(roads.nonEmpty) {
      selected_road_element -= 1
      if(selected_road_element <= -2) selected_road_element = roads.length-1
    } else selected_road_element = -1
  })

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouse(onBtnDown = m => {
    _road_start match {
      case Some(from) =>
        roads += new TwoLinesEachSide(from, nearestPoint(scaledCoord(m)))
        _road_start = None
      case None =>
        _road_start = Some(nearestPoint(scaledCoord(m)))
    }
  })
  rightMouse(onBtnDown = m => {
    _road_start match {
      case Some(from) =>
        _road_start = None
      case None =>
        if(selected_road_element != -1 && roads.nonEmpty && selected_road_element >= 0 && selected_road_element < roads.length) {
          roads.remove(selected_road_element)
          if(selected_road_element >= roads.length) selected_road_element = -1
        }
    }
  })

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 1) globalScale -= 1
    else if(globalScale > 0.1f) globalScale -= 0.1f
  })

  center = _center

  render {
    (-5000 to 5000 by 15).foreach(x => drawLine(Vec(x, -5000), Vec(x, 5000), DARK_GRAY))
    (-5000 to 5000 by 15).foreach(y => drawLine(Vec(-5000, y), Vec(5000, y), DARK_GRAY))
    roads.zipWithIndex.foreach {
      case (road, idx) =>
        if(idx == selected_road_element) {
          road.drawSelf(RED)
        } else {
          road.drawSelf(WHITE)
        }
    }
    if(_road_start.nonEmpty) {
      drawLine(_road_start.get, nearestPoint(scaledCoord(mouseCoord)), WHITE)
    }
  }

  def nearestPoint(v:Vec):Vec = {
    Vec(v.ix/15*15-5, v.iy/15*15-5)
  }
}

sealed trait RoadElement {
  def drawSelf(color:ScageColor)
}
class TwoLinesEachSide(from:Vec, to:Vec) extends RoadElement {
  override def drawSelf(color: ScageColor) {
    val n = (to - from).n
    val n_turn_1 = n.rotateDeg(90)
    val n_turn_2 = n.rotateDeg(-90)

    // двойная сплошная по центру
    drawLine(from+n_turn_1, to+n_turn_1, color)
    drawLine(from+n_turn_2, to+n_turn_2, color)

    // пунктирные линии
    drawDashedLine(from+n_turn_1*15, to+n_turn_1*15, 5, color)
    drawDashedLine(from+n_turn_2*15, to+n_turn_2*15, 5, color)

    // границы дороги
    drawLine(from+n_turn_1*30, to+n_turn_1*30, color)
    drawLine(from+n_turn_2*30, to+n_turn_2*30, color)
  }
}

class RoadTurn90Deg(pos:Vec, var turn_dir:Int) extends RoadElement {
  override def drawSelf(color: ScageColor): Unit = {
    turn_dir match {
      case 1 =>

      case 2 =>
      case _ =>
    }
  }
}