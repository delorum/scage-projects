package net.scage.blases.levels

import net.scage.support.physics.objects.StaticPolygon
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.{SpeedPolygon, BurstPolygon, Level}
import net.scage.blases.Relatives._
import net.scage.support.Vec

object Level4 extends Level {
  def load() {
    val first = new StaticPolygon(rVec(654, 719), rVec(447, 758), rVec(1001, 758), rVec(1001, 431), rVec(767, 637), rVec(801, 696))
    val second = new StaticPolygon(rVec(495, 611), rVec(708, 470), rVec(721, 636), rVec(661, 671))
    val third = new StaticPolygon(rVec(496, 578), rVec(505, 449), rVec(656, 472))
    val fourth = new StaticPolygon(rVec(50, 535), rVec(375, 493), rVec(368, 462), rVec(43, 508))
    val fifth = new StaticPolygon(rVec(526, 334), rVec(562, 351), rVec(666, 160), rVec(635, 143))
    val sixth = new StaticPolygon(rVec(7, 280), rVec(146, 240), rVec(154, 265), rVec(16, 308))
    val seventh = new StaticPolygon(rVec(279, 118), rVec(384, 6), rVec(405, 27), rVec(298, 137))

    val speed_polygon = new SpeedPolygon(List(rVec(132, 204), rVec(226, 111), rVec(554, 378), rVec(450, 487)), (rVec(554, 378) - rVec(226, 111)))

    val burst_polygon1 = new BurstPolygon(rVec(43, 190), rVec(199, 56), rVec(222, 77), rVec(69, 215))
    val burst_polygon2 = new BurstPolygon(rVec(584, 365), rVec(607, 363), rVec(611, 434), rVec(585, 437))
    val burst_polygon3 = new BurstPolygon(rVec(355, 534), rVec(440, 570), rVec(448, 546), rVec(369, 513))

    physics.addPhysicals(first, second, third, fourth, fifth, sixth, seventh)

    def avg(points:Array[Vec]):Vec = Vec(points.map(_.x).sum/points.length, points.map(_.y).sum/points.length)
    
    val render_id = render {
      currentColor = WHITE
      drawPolygon(first.points)
      //print("first", avg(first.points))
      
      drawPolygon(second.points)
      //print("second", avg(second.points))

      drawPolygon(third.points)
      //print("third", avg(third.points))

      drawPolygon(fourth.points)
      //print("fourth", avg(fourth.points))

      drawPolygon(fifth.points)
      //print("fifth", avg(fifth.points))

      drawPolygon(sixth.points)
      //print("sixth", avg(sixth.points))

      drawPolygon(seventh.points)
      //print("seventh", avg(seventh.points))

      /*drawPolygon(eighth.points)
      print("eighth", avg(eighth.points))

      drawPolygon(ninth.points)
      print("ninth", avg(ninth.points))*/

      /*drawPolygon(tenth.points)
      print("tenth", avg(tenth.points))

      drawPolygon(eleventh.points)
      print("eleventh", avg(eleventh.points))*/

      drawStartFinish()
    }

    clear {
      physics.removePhysicals(first, second, third, fourth, fifth, sixth, seventh)
      speed_polygon.remove()
      burst_polygon1.remove()
      burst_polygon2.remove()
      burst_polygon3.remove()
      delOperation(render_id)
      deleteSelf()
    }

    /*interface {
      print(mouseCoord, 20, 20, GREEN)
    }*/
  }

  def startCoord = rVec(70, 80)
  def finishCoord = rVec(725, 675)
}
