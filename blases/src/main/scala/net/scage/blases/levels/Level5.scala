package net.scage.blases.levels

import net.scage.support.physics.objects.StaticPolygon
import net.scage.ScageLib._
import net.scage.blases.Blases._
import net.scage.blases.{SpeedPolygon, BurstPolygon, Level}
import net.scage.blases.Relatives._
import net.scage.support.Vec

object Level5 extends Level {
  def load() {
    val first = new StaticPolygon(rVec(286, 702), rVec(1, 704), rVec(2, 679), rVec(289, 680))
    val second = new StaticPolygon(rVec(403, 699), rVec(424, 730), rVec(450, 698), rVec(449, 662), rVec(403, 663))
    val third = new StaticPolygon(rVec(523, 761), rVec(589, 760), rVec(588, 151), rVec(516, 152))
    val fourth = new StaticPolygon(rVec(368, 631), rVec(402, 596), rVec(194, 460), rVec(169, 496))
    val fifth = new StaticPolygon(rVec(45, 401), rVec(222, 400), rVec(221, 352), rVec(39, 353))
    val sixth = new StaticPolygon(rVec(662, 317), rVec(802, 215), rVec(960, 336))
    val seventh = new StaticPolygon(rVec(605, 424), rVec(673, 424), rVec(705, 506), rVec(799, 520), rVec(829, 598), rVec(740, 628), rVec(658, 583), rVec(676, 532))
    val eighth = new StaticPolygon(rVec(789, 429), rVec(955, 594), rVec(819, 700), rVec(930, 743), rVec(991, 686), rVec(995, 510), rVec(908, 433))

    physics.addPhysicals(first, second, third, fourth, fifth, sixth, seventh, eighth)

    val burst_polygon = new BurstPolygon(rVec(490, 614), rVec(512, 604), rVec(479, 547), rVec(457, 560))
    val speed_polygon = new SpeedPolygon(List(rVec(504, 12), rVec(556, 63), rVec(318, 307), rVec(259, 248)), (rVec(556, 63) - rVec(318, 307)))

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

      drawPolygon(eighth.points)
      //print("eighth", avg(eighth.points))

      /*drawPolygon(ninth.points)
      print("ninth", avg(ninth.points))

      drawPolygon(tenth.points)
      print("tenth", avg(tenth.points))*/

      /*drawPolygon(eleventh.points)
      print("eleventh", avg(eleventh.points))*/

      drawStartFinish()
    }

    interface {
      print(mouseCoord, 20, 20, GREEN)
    }

    clear {
      physics.removePhysicals(first, second, third, fourth, fifth, sixth, seventh, eighth)
      speed_polygon.remove()
      burst_polygon.remove()
      /*burst_polygon2.remove()
      burst_polygon3.remove()*/
      delOperation(render_id)
      deleteSelf()
    }
  }

  def startCoord = rVec(65, 732)
  def finishCoord = rVec(630, 527)
}
