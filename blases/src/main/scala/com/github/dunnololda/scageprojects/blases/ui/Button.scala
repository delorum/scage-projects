package com.github.dunnololda.scageprojects.blases.ui

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.blases.Relatives._

class Button(message: => String,
             coord: Vec,
             width: Int,
             screen: Screen with MultiController,
             onBtnPressed: => Any,
             color:ScageColor = BLACK,
             var visible:Boolean = true)/* extends IntersectablePolygon */{
  def intersectableVertices = List(rVec(coord) + Vec(-5, 20),
                                   rVec(coord) + Vec(-5 + width, 20),
                                   rVec(coord) + Vec(-5 + width, -10),
                                   rVec(coord) + Vec(-5, -10))

  screen.interface {
    if(visible) {
      print(message, rVec(coord), color)
    }
  }

  screen.leftMouseOnAreaIgnorePause(intersectableVertices, onBtnDown = {m => if(visible/* && containsCoord(m)*/) onBtnPressed})
}
