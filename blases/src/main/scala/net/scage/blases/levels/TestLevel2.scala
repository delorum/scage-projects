package net.scage.blases.levels

import net.scage.blases.Level
import net.scage.blases.levelparts.Star
import net.scage.blases.Relatives._

object TestLevel2 extends Level {
  def constructLevel() {
    new Star(rVec(512, 384), 7, rInt(30), rInt(15))
  }

  def startCoord = rVec(85, 426)
  def finishCoords = List(rVec(987, 444))
}
