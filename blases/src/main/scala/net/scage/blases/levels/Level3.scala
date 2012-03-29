package net.scage.blases.levels

import net.scage.blases.Relatives._
import net.scage.blases.levelparts.{SimpleObstacle, SpeedPolygon, SpikesPolygon}
import net.scage.blases.Level

object Level3 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(304, 750), rVec(256, 614), rVec(286, 608), rVec(332, 745))
    new SimpleObstacle(rVec(623, 210), rVec(586, 244), rVec(555, 207), rVec(555, 163), rVec(621, 162))
    new SimpleObstacle(rVec(499, 491), rVec(549, 491), rVec(550, 287), rVec(497, 287))
    new SimpleObstacle(rVec(255, 611), rVec(661, 534), rVec(656, 521), rVec(252, 600))
    new SimpleObstacle(rVec(884, 327), rVec(916, 306), rVec(773, 101), rVec(744, 129))
    new SimpleObstacle(rVec(468, 186), rVec(405, 115), rVec(534, 119))
    new SimpleObstacle(rVec(117, 572), rVec(403, 370), rVec(380, 343), rVec(97, 550))
    new SimpleObstacle(rVec(864, 525), rVec(915, 354), rVec(943, 365), rVec(884, 533))

    new SpeedPolygon(List(rVec(740, 520), rVec(645, 204), rVec(773, 169), rVec(860, 493)), (rVec(871, 397) - rVec(742, 79)))
    new SpikesPolygon(rVec(777, 569), rVec(767, 549), rVec(844, 527), rVec(856, 553))
  }

  def startCoord = rVec(153, 609)
  def finishCoord = rVec(380, 653)
}
