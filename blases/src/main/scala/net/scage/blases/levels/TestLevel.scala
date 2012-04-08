package net.scage.blases.levels

import net.scage.blases.Level
import net.scage.blases.levelparts.MovingSpikes
import net.scage.support.Vec
import net.scage.ScageLib._
import net.scage.blases.Relatives._

object TestLevel extends Level {
  def constructLevel() {
    new MovingSpikes(Vec(100, windowHeight/4), Vec(100, 3*windowHeight/4), 1)
  }

  def startCoord = rVec(134, 52)
  def finishCoord = rVec(746, 725)
}
