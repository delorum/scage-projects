package net.scage.blases.levels

import net.scage.support.physics.objects.StaticPolygon
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.{SpeedPolygon, BurstPolygon, Level}
import net.scage.blases.Relatives._

object Level3 extends Level {
  def load() {
    val first = new StaticPolygon(rVec(58, 526), rVec(309, 292), rVec(284, 264), rVec(37, 505))
    val second = new StaticPolygon(rVec(307, 105), rVec(332, 31), rVec(249, 36))
    val third = new StaticPolygon(rVec(432, 365), rVec(468, 365), rVec(468, 130), rVec(432, 130))
    val fourth = new StaticPolygon(rVec(601, 287), rVec(639, 240), rVec(633, 185), rVec(582, 178), rVec(571, 232))
    val fifth = new StaticPolygon(rVec(738, 44), rVec(893, 209), rVec(924, 188), rVec(763, 25))
    val sixth = new StaticPolygon(rVec(892, 233), rVec(892, 429), rVec(927, 429), rVec(927, 233))
    val seventh = new StaticPolygon(rVec(254, 561), rVec(684, 428), rVec(670, 401), rVec(248, 541))
    val eighth = new StaticPolygon(rVec(258, 578), rVec(308, 748), rVec(350, 739), rVec(299, 568))

    physics.addPhysicals(first, second, third, fourth, fifth, sixth, seventh, eighth)

    val speed_polygon = new SpeedPolygon(List(rVec(764, 438), rVec(871, 397), rVec(742, 79), rVec(636, 125)), (rVec(871, 397) - rVec(742, 79)))
    val burst_polygon = new BurstPolygon(rVec(756, 535), rVec(898, 474), rVec(885, 445), rVec(738, 507))

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
  def finishCoord = rVec(410, 608)
}
