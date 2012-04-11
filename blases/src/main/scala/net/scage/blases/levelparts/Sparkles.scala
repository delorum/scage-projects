package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.blases.IntersectablePolygon
import net.scage.blases.Blases._
import net.scage.ScageLib._
import collection.mutable.ArrayBuffer
import net.scage.blases.Relatives._

class Sparkles(from:Vec, to:Vec, height:Int, active_period:Int, inactive_period:Int) extends IntersectablePolygon {
  private val ang = 360 - ((to - from).deg(Vec(0,1)) - 90).toFloat
  val intersectableVertices = List(from+Vec(0, height/2).rotateDeg(ang),
                                   to+Vec(0, height/2).rotateDeg(ang),
                                   to+Vec(0, -height/2).rotateDeg(ang),
                                   from+Vec(0, -height/2).rotateDeg(ang))

  def sparkleLine(from:Vec, to:Vec) = {
    val line = to - from
    val step_n = line.n.rotate(math.Pi/2)
    val result = ArrayBuffer(from, from+line*0.05f)
    var side = 1
    var point = 0.05f
    while(point < 0.95f) {
      point += math.min((math.random*0.05).toFloat, 0.95f)
      result ++= Array(result.last, (from + line*point + step_n*(math.random*20).toFloat*side))
      side *= -1
    }
    result ++= Array(result.last, to)
    result.toList
  }
  private var sparkle_line = sparkleLine(from, to)

  private val render_id = render {
    openglLocalTransform {
      openglMove(from)
      openglRotate(ang)
      drawRectCentered(Vec.zero, 10, height, rColor(WHITE))
    }
    openglLocalTransform {
      openglMove(to)
      openglRotate(ang)
      drawRectCentered(Vec.zero, 10, height, rColor(WHITE))
    }
    if(is_active) drawLines(sparkle_line, rColor(RED))
  }

  val pew = render {
    vertices_points.foreach(v => {
      drawRectCentered(tracer.pointCenter(v), tracer.h_x, tracer.h_y, DARK_GRAY)
      drawFilledCircle(tracer.pointCenter(v), 3, YELLOW)
      //print(point.ix+":"+point.iy, tracer.pointCenter(point))
    })
    drawPolygon(intersectableVertices, GREEN)
  }

  clear {
    delOperations(pew, currentOperation)
  }

  private var is_active = false
  private var start = System.currentTimeMillis
  private val action_id = action {
    if(is_active) {
      forEachBlaseInside {blase =>
        blase.burst()
        score_for_level -= 100
        new FlyingWord(-100, YELLOW, blase.location, blase.velocity)
      }
      if(System.currentTimeMillis - start > active_period) {
        is_active = false
        start = System.currentTimeMillis
      }
    } else {
      if(System.currentTimeMillis - start > inactive_period) {
        is_active = true
        start = System.currentTimeMillis
        sparkle_line = sparkleLine(from, to)
      }
    }
  }

  clear {
    delOperations(action_id, render_id, currentOperation)
  }
}
