package com.github.dunnololda.scageprojects.blases.levels

import com.github.dunnololda.scageprojects.blases.Level
import com.github.dunnololda.scageprojects.blases.levelparts.{SpeedPolygon, SimpleObstacle}
import com.github.dunnololda.scageprojects.blases.Relatives._

object Level2 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(86, 526), rVec(353, 526), rVec(353, 414), rVec(86, 414))
    new SimpleObstacle(rVec(625, 715), rVec(729, 715), rVec(730, 414), rVec(625, 414))
    new SimpleObstacle(rVec(227, 311), rVec(502, 311), rVec(502, 280), rVec(227, 280))
    new SimpleObstacle(rVec(730, 212), rVec(779, 170), rVec(779, 105), rVec(682, 105), rVec(682, 170))
    new SimpleObstacle(rVec(568, 143), rVec(594, 124), rVec(511, 17), rVec(487, 38))

    new SpeedPolygon(List(rVec(415, 538), rVec(523, 621), rVec(737, 364), rVec(639, 284)), rVec(737, 364) - rVec(523, 621))
  }

  def startCoord = rVec(183, 630)
  def finishCoords = List(rVec(855, 58))
}
