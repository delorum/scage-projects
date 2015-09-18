package com.github.dunnololda.scageprojects.blases.levels

object TestLevel2 extends Level {
  def constructLevel() {
    new Star(rVec(512, 384))
  }

  def startCoord = rVec(85, 426)
  def finishCoords = List(rVec(987, 444))
}
