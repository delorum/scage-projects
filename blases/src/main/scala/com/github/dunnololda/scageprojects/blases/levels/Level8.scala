package com.github.dunnololda.scageprojects.blases.levels

import com.github.dunnololda.scageprojects.blases.Level
import com.github.dunnololda.scageprojects.blases.levelparts._
import com.github.dunnololda.scageprojects.blases.Relatives._

object Level8 extends Level {
  def constructLevel() {
    new SimpleObstacle(rVec(429, 229), rVec(7, 520), rVec(15, 347), rVec(559, 80))
    new SimpleObstacle(rVec(640, 210), rVec(190, 701), rVec(209, 728), rVec(293, 701), rVec(602, 403))
    new SimpleObstacle(rVec(655, 655), rVec(753, 660), rVec(785, 728), rVec(748, 757), rVec(844, 751), rVec(797, 635), rVec(648, 626))
    new SimpleObstacle(rVec(803, 164), rVec(981, 365), rVec(1008, 349), rVec(829, 134))

    new SpeedPolygon(List(rVec(491, 215), rVec(554, 260), rVec(171, 684), rVec(119, 641)), rVec(119, 641) - rVec(491, 215))
    new SpeedPolygon(List(rVec(562, 476), rVec(812, 593), rVec(840, 552), rVec(592, 442)), rVec(812, 593) - rVec(562, 476))
    new SpeedPolygon(List(rVec(882, 689), rVec(960, 688), rVec(959, 361), rVec(882, 360)), rVec(882, 360) - rVec(882, 689))

    new SpikesPolygon(rVec(557, 110), rVec(652, 195), rVec(670, 177), rVec(574, 93))
    new SpikesPolygon(rVec(631, 673), rVec(660, 710), rVec(682, 710), rVec(652, 659))

    new Star(rVec(719, 701))
    new Star(rVec(144, 129))

    new MovingSpikes(rVec(662, 360), rVec(817, 235))
  }

  def startCoord = rVec(546, 213)
  def finishCoords = List(rVec(41, 269))
}

