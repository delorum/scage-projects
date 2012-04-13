package net.scage.blases.levels

import net.scage.blases.Level
import net.scage.support.Vec
import net.scage.ScageLib._
import net.scage.blases.Relatives._
import net.scage.blases.levelparts.{MovingObstacle, Sparkles, MovingSpikes}

object TestLevel extends Level {
  def constructLevel() {
    new MovingSpikes(rVec(100, 576), rVec(250, 192))
    new Sparkles(rVec(300, 576), rVec(500, 192), 40, 1000, 3000)
    new MovingObstacle(List(rVec(600, 576), rVec(700, 576), rVec(700, 556), rVec(600, 556)), rVec(600, 576), rVec(600, 192))
  }

  def startCoord = rVec(134, 52)
  def finishCoords = List(rVec(746, 725))
}
