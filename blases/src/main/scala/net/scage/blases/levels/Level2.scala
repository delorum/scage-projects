package net.scage.blases.levels

import net.scage.support.physics.objects.StaticPolygon
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.{SpeedPolygon, Level}
import net.scage.blases.Relatives._

object Level2 extends Level {
  def load() {
    val first = new StaticPolygon(rVec(86, 526), rVec(353, 526), rVec(353, 414), rVec(86, 414))
    val second = new StaticPolygon(rVec(625, 715), rVec(729, 715), rVec(730, 414), rVec(625, 414))
    val third = new StaticPolygon(rVec(227, 311), rVec(502, 311), rVec(502, 280), rVec(227, 280))
    val fourth = new StaticPolygon(rVec(730, 212), rVec(779, 170), rVec(779, 105), rVec(682, 105), rVec(682, 170))
    val fifth = new StaticPolygon(rVec(568, 143), rVec(594, 124), rVec(511, 17), rVec(487, 38))

    physics.addPhysicals(first, second, third, fourth, fifth)

    val speed_polygon = new SpeedPolygon(List(rVec(415, 538), rVec(523, 621), rVec(737, 364), rVec(639, 284)), (rVec(737, 364) - rVec(523, 621)))

    val render_id = render {
      currentColor = WHITE
      drawPolygon(first.points)
      drawPolygon(second.points)
      drawPolygon(third.points)
      drawPolygon(fourth.points)
      drawPolygon(fifth.points)

      drawStartFinish()
    }

    clear {
      physics.removePhysicals(first, second, third, fourth, fifth)
      speed_polygon.remove()
      delOperation(render_id)
      deleteSelf()
    }
  }

  def startCoord = rVec(183, 630)
  def finishCoord = rVec(855, 58)
}
