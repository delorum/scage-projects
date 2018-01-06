package com.github.dunnololda.scageprojects.blases.levels

import com.github.dunnololda.scageprojects.blases.Level
import com.github.dunnololda.scageprojects.blases.levelparts.{MovingObstacle, Sparkles, MovingSpikes}
import com.github.dunnololda.scageprojects.blases.Relatives._

object TestLevel extends Level {
  def constructLevel() {
    new MovingSpikes(rVec(100, 576), rVec(250, 192))
    new Sparkles(rVec(300, 576), rVec(500, 192), 40, 1000, 3000)
    new MovingObstacle(List(rVec(600, 576), rVec(700, 576), rVec(700, 556), rVec(600, 556)), rVec(600, 576), rVec(600, 192))
  }

  def startCoord = rVec(134, 52)
  def finishCoords = List(rVec(746, 725))
}
