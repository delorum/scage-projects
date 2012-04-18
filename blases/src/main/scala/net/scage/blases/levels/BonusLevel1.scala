package net.scage.blases.levels

import net.scage.blases.Relatives._
import net.scage.blases.levelparts.{Star, SpeedPolygon, SpikesPolygon, SimpleObstacle}
import net.scage.blases.{BonusLevel, Level}
import net.scage.blases.Blases._
import net.scage.ScageLib._

object BonusLevel1 extends BonusLevel {
  def bonusCondition = blases_shot <= 30
  def bonusConditionDescription = xml("bonus1.condition")

  def constructLevel() {
    new SimpleObstacle(rVec(393, 537), rVec(249, 700), rVec(85, 682), rVec(196, 572))
    new SimpleObstacle(rVec(460, 540), rVec(361, 686), rVec(510, 684))
    new SimpleObstacle(rVec(527, 541), rVec(615, 704), rVec(902, 705))
    new SimpleObstacle(rVec(227, 440), rVec(840, 439), rVec(839, 420), rVec(228, 421))
    new SimpleObstacle(rVec(42, 363), rVec(209, 363), rVec(71, 332), rVec(95, 249), rVec(42, 329))
    new SimpleObstacle(rVec(234, 196), rVec(295, 132), rVec(258, 100), rVec(102, 241))
    new SimpleObstacle(rVec(236, 315), rVec(383, 234), rVec(361, 194), rVec(259, 232))
    new SimpleObstacle(rVec(891, 404), rVec(926, 295), rVec(967, 407))
    new SimpleObstacle(rVec(408, 94), rVec(920, 274), rVec(935, 248), rVec(426, 68))
    new SimpleObstacle(rVec(408, 94), rVec(920, 274), rVec(935, 248), rVec(426, 68))

    new SpeedPolygon(List(rVec(850, 541), rVec(949, 458), rVec(985, 500), rVec(890, 581)), (rVec(985, 500) - rVec(890, 581)))
    new SpeedPolygon(List(rVec(248, 508), rVec(248, 450), rVec(560, 450), rVec(560, 509)), (rVec(560, 450) - rVec(248, 450)))
    new SpeedPolygon(List(rVec(249, 410), rVec(840, 411), rVec(839, 310), rVec(249, 311)), (rVec(249, 410) - rVec(840, 411)))
    new SpeedPolygon(List(rVec(420, 230), rVec(419, 189), rVec(669, 189), rVec(667, 230)), (rVec(667, 230) - rVec(420, 230)))

    new SpikesPolygon(rVec(350, 113), rVec(401, 171), rVec(429, 150), rVec(377, 95))
    new SpikesPolygon(rVec(588, 451), rVec(627, 451), rVec(628, 550), rVec(589, 550))
    new SpikesPolygon(rVec(941, 628), rVec(1000, 692), rVec(1020, 679), rVec(966, 616))

    new Star(rVec(161, 718))
    new Star(rVec(373, 607))
    new Star(rVec(528, 618))
    new Star(rVec(170, 281))
    new Star(rVec(137, 134))
  }

  def startCoord = rVec(85, 426)
  def finishCoords = List(rVec(987, 444))
}
