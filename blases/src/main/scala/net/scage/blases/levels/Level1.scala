package net.scage.blases.levels

import net.scage.blases.Relatives._
import net.scage.blases.levelparts.{SimpleObstacle, SpikesPolygon}
import net.scage.blases.Level

object Level1 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(84, 212), rVec(458, 564), rVec(627, 393), rVec(591, 359), rVec(454, 495), rVec(113, 175))
    new SimpleObstacle(rVec(76, 85), rVec(810, 83), rVec(812, 46), rVec(77, 45))
    new SimpleObstacle(rVec(782, 658), rVec(829, 658), rVec(830, 151), rVec(787, 152))
    new SimpleObstacle(rVec(562, 281), rVec(615, 256), rVec(644, 186), rVec(568, 150), rVec(536, 223))

    new SpikesPolygon(rVec(772, 153), rVec(787, 140), rVec(745, 80), rVec(731, 95))
  }

  def startCoord = rVec(271, 564)
  def finishCoord = rVec(350, 300)
}
