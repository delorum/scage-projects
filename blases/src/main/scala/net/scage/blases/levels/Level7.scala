package net.scage.blases.levels

import net.scage.blases.Level
import net.scage.blases.Relatives._
import net.scage.blases.levelparts._

object Level7 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(850, 710), rVec(932, 635), rVec(786, 558))
    new SimpleObstacle(rVec(788, 710), rVec(219, 709), rVec(220, 511), rVec(595, 496))
    new SimpleObstacle(rVec(358, 312), rVec(153, 239), rVec(201, 124), rVec(334, 135))

    new SpeedPolygon(List(rVec(413, 381), rVec(642, 381), rVec(642, 115), rVec(414, 115)), (rVec(414, 115)-rVec(413, 381)))
    new SpeedPolygon(List(rVec(840, 762), rVec(225, 762), rVec(225, 724), rVec(841, 723)), (rVec(225, 724)-rVec(841, 723)))
    new SpeedPolygon(List(rVec(199, 760), rVec(18, 759), rVec(19, 430), rVec(199, 430)), (rVec(19, 430)-rVec(18, 759)))

    new SpikesPolygon(rVec(389, 100), rVec(688, 99), rVec(689, 44), rVec(389, 45))
    new SpikesPolygon(rVec(18, 411), rVec(202, 411), rVec(300, 366), rVec(265, 348), rVec(194, 380), rVec(20, 380))
    new SpikesPolygon(rVec(861, 431), rVec(996, 528), rVec(1010, 505), rVec(866, 405))

    new Star(rVec(128, 348))
  }

  def startCoord = rVec(965, 738)
  def finishCoords = List(rVec(83, 152), rVec(902, 170))
}
