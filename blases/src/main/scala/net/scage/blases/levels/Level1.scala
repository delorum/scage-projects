package net.scage.blases.levels

import net.scage.support.physics.objects.StaticPolygon
import net.scage.blases.{BurstPolygon, Level}
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.Relatives._

object Level1 extends Level {
  def load() {
    val first = new StaticPolygon(rVec(84, 212), rVec(458, 564), rVec(627, 393), rVec(591, 359), rVec(454, 495), rVec(113, 175))
    val second = new StaticPolygon(rVec(76, 85), rVec(810, 83), rVec(812, 46), rVec(77, 45))
    val third = new StaticPolygon(rVec(782, 658), rVec(829, 658), rVec(830, 151), rVec(787, 152))
    val fourth = new StaticPolygon(rVec(562, 281), rVec(615, 256), rVec(644, 186), rVec(568, 150), rVec(536, 223))

    physics.addPhysicals(first, second, third, fourth)

    val burst_polygon = new BurstPolygon(rVec(772, 153), rVec(787, 140), rVec(745, 80), rVec(731, 95))

    val render_id = render {
      currentColor = WHITE
      drawPolygon(first.points)
      drawPolygon(second.points)
      drawPolygon(third.points)
      drawPolygon(fourth.points)

      drawStartFinish()
    }

    clear {
      physics.removePhysicals(first, second, third, fourth)
      burst_polygon.remove()
      delOperation(render_id)
      deleteSelf()
    }
  }

  def startCoord = rVec(271, 564)
  def finishCoord = rVec(350, 300)
}
