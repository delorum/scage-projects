package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.blases.IntersectablePolygon
import net.scage.blases.Blases._
import net.scage.ScageLib._

class Sparkles(from:Vec, to:Vec,  height:Int, active_period:Int, inactive_period:Int) extends IntersectablePolygon {
  val vertices = List(from+Vec(0, height/2), to+Vec(0, height/2), to+Vec(0, -height/2), from+Vec(0, -height/2))

  private val render_id = render {
    drawRectCentered(from, 10, height, WHITE)
    drawRectCentered(to, 10, height, WHITE)
    drawLines(List(from+Vec(0, height/2), to+Vec(0, height/2), from+Vec(0, -height/2), to+Vec(0, -height/2)), RED)
  }

  private var is_active = false
  private var start = System.currentTimeMillis
  def inactive() {
    is_active = false
    start = System.currentTimeMillis
    action {
      if(System.currentTimeMillis - start > inactive_period) {
        active()
        deleteSelf()
      }
    }
  }

  def active() {
    is_active = true
    start = System.currentTimeMillis
    action {
      if(System.currentTimeMillis - start > active_period) {
        inactive()
        deleteSelf()
      }
    }
  }


}
