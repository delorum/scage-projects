package com.github.dunnololda.scageprojects.blases.levels

import com.github.dunnololda.scageprojects.blases.Level
import com.github.dunnololda.scageprojects.blases.levelparts.Star
import com.github.dunnololda.scageprojects.blases.Relatives._

object TestLevel2 extends Level {
  def constructLevel() {
    new Star(rVec(512, 384))
  }

  def startCoord = rVec(85, 426)
  def finishCoords = List(rVec(987, 444))
}
