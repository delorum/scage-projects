package com.github.dunnololda.scageprojects.blases.levels

import com.github.dunnololda.scageprojects.blases.Level
import com.github.dunnololda.scageprojects.blases.levelparts.{SpeedPolygon, SpikesPolygon, SimpleObstacle}
import com.github.dunnololda.scageprojects.blases.Relatives._

object Level5 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(286, 702), rVec(1, 704), rVec(2, 679), rVec(289, 680))
    new SimpleObstacle(rVec(403, 699), rVec(424, 730), rVec(450, 698), rVec(449, 662), rVec(403, 663))
    new SimpleObstacle(rVec(523, 761), rVec(589, 760), rVec(588, 151), rVec(516, 152))
    new SimpleObstacle(rVec(368, 631), rVec(402, 596), rVec(194, 460), rVec(169, 496))
    new SimpleObstacle(rVec(45, 401), rVec(222, 400), rVec(221, 352), rVec(39, 353))
    new SimpleObstacle(rVec(662, 317), rVec(802, 215), rVec(960, 336))
    new SimpleObstacle(rVec(605, 424), rVec(673, 424), rVec(705, 506), rVec(799, 520), rVec(829, 598), rVec(740, 628), rVec(658, 583), rVec(676, 532))
    new SimpleObstacle(rVec(789, 429), rVec(955, 594), rVec(819, 700), rVec(930, 743), rVec(991, 686), rVec(995, 510), rVec(908, 433))

    new SpikesPolygon(rVec(490, 614), rVec(512, 604), rVec(479, 547), rVec(457, 560))
    new SpeedPolygon(List(rVec(504, 12), rVec(556, 63), rVec(318, 307), rVec(259, 248)), rVec(556, 63) - rVec(318, 307))
  }

  def startCoord = rVec(200, 732)
  def finishCoords = List(rVec(630, 527))
}
