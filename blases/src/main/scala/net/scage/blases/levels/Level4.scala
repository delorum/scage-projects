package net.scage.blases.levels

import net.scage.blases.Relatives._
import net.scage.blases.levelparts.{SimpleObstacle, SpeedPolygon, SpikesPolygon}
import net.scage.blases.Level

object Level4 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(654, 719), rVec(447, 758), rVec(1001, 758), rVec(1001, 431), rVec(767, 637), rVec(801, 696))
    new SimpleObstacle(rVec(495, 611), rVec(708, 470), rVec(721, 636), rVec(661, 671))
    new SimpleObstacle(rVec(496, 578), rVec(505, 449), rVec(656, 472))
    new SimpleObstacle(rVec(50, 535), rVec(375, 493), rVec(368, 462), rVec(43, 508))
    new SimpleObstacle(rVec(526, 334), rVec(562, 351), rVec(666, 160), rVec(635, 143))
    new SimpleObstacle(rVec(7, 280), rVec(146, 240), rVec(154, 265), rVec(16, 308))
    new SimpleObstacle(rVec(279, 118), rVec(384, 6), rVec(405, 27), rVec(298, 137))
    new SimpleObstacle(rVec(43, 190), rVec(199, 56), rVec(222, 77), rVec(69, 215))

    new SpeedPolygon(List(rVec(132, 204), rVec(226, 111), rVec(554, 378), rVec(450, 487)), (rVec(554, 378) - rVec(226, 111)))

    new SpikesPolygon(rVec(584, 365), rVec(607, 363), rVec(611, 434), rVec(585, 437))
    new SpikesPolygon(rVec(355, 534), rVec(440, 570), rVec(448, 546), rVec(369, 513))
  }

  def startCoord = rVec(70, 80)
  def finishCoord = rVec(725, 675)
}
