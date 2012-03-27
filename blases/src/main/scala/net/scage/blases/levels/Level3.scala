package net.scage.blases.levels

import net.scage.support.physics.objects.StaticPolygon
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.{SpeedPolygon, BurstPolygon, Level}
import net.scage.blases.Relatives._

object Level3 extends Level {
  def load() {
    val first = new StaticPolygon(rVec(304, 750), rVec(256, 614), rVec(286, 608), rVec(332, 745))
    val second = new StaticPolygon(rVec(623, 210), rVec(586, 244), rVec(555, 207), rVec(555, 163), rVec(621, 162))
    val third = new StaticPolygon(rVec(499, 491), rVec(549, 491), rVec(550, 287), rVec(497, 287))
    val fourth = new StaticPolygon(rVec(255, 611), rVec(661, 534), rVec(656, 521), rVec(252, 600))
    val fifth = new StaticPolygon(rVec(884, 327), rVec(916, 306), rVec(773, 101), rVec(744, 129))
    val sixth = new StaticPolygon(rVec(468, 186), rVec(405, 115), rVec(534, 119))
    val seventh = new StaticPolygon(rVec(117, 572), rVec(403, 370), rVec(380, 343), rVec(97, 550))
    val eighth = new StaticPolygon(rVec(864, 525), rVec(915, 354), rVec(943, 365), rVec(884, 533))

    physics.addPhysicals(first, second, third, fourth, fifth, sixth, seventh, eighth)

    val speed_polygon = new SpeedPolygon(List(rVec(740, 520), rVec(645, 204), rVec(773, 169), rVec(860, 493)), (rVec(871, 397) - rVec(742, 79)))
    val burst_polygon = new BurstPolygon(rVec(777, 569), rVec(767, 549), rVec(844, 527), rVec(856, 553))

    val render_id = render {
      currentColor = WHITE
      drawPolygon(first.points)
      drawPolygon(second.points)
      drawPolygon(third.points)
      drawPolygon(fourth.points)
      drawPolygon(fifth.points)
      drawPolygon(sixth.points)
      drawPolygon(seventh.points)
      drawPolygon(eighth.points)

      drawStartFinish()
    }

    clear {
      physics.removePhysicals(first, second, third, fourth, fifth, sixth, seventh, eighth)
      speed_polygon.remove()
      burst_polygon.remove()
      delOperation(render_id)
      deleteSelf()
    }
  }

  def startCoord = rVec(153, 609)
  def finishCoord = rVec(380, 653)
}
