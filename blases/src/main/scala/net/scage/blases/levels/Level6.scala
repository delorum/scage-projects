package net.scage.blases.levels

import net.scage.blases.Relatives._
import net.scage.blases.levelparts.{SimpleObstacle, SpeedPolygon, SpikesPolygon}
import net.scage.blases.Level

object Level6 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(512, 35), rVec(974, 683), rVec(1011, 664), rVec(548, 16))
    new SimpleObstacle(rVec(583, 427), rVec(667, 545), rVec(734, 509), rVec(653, 386))
    new SimpleObstacle(rVec(338, 765), rVec(658, 764), rVec(657, 743), rVec(337, 742))
    new SimpleObstacle(rVec(777, 690), rVec(847, 763), rVec(938, 763), rVec(937, 690))
    new SimpleObstacle(rVec(41, 245), rVec(102, 98), rVec(441, 292))
    
    new SpeedPolygon(List(rVec(178, 592), rVec(827, 592), rVec(827, 680), rVec(177, 681)), (rVec(827, 592) - rVec(178, 592)))
    new SpeedPolygon(List(rVec(596, 201), rVec(281, 470), rVec(362, 557), rVec(675, 292)), (rVec(281, 470) - rVec(596, 201)))
    
    new SpikesPolygon(rVec(434, 200), rVec(433, 144), rVec(479, 143), rVec(479, 196), rVec(454, 246))
  }
  
  def startCoord = rVec(134, 52)
  def finishCoords = List(rVec(746, 725))
}
