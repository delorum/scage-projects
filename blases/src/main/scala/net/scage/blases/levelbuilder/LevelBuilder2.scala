package net.scage.blases.levelbuilder

import net.scage.ScageScreenApp
import net.scage.support.Vec
import collection.mutable.ArrayBuffer
import net.scage.ScageLib._
import net.scage.support.tracer3.{Trace, CoordTracer}
import net.scage.support.physics.objects.StaticPolygon

object LevelBuilder2 extends ScageScreenApp("Level Builder 2") {
  private val polygons = ArrayBuffer[List[Vec]]()
  val tracer = CoordTracer()

  private var last_vertice = Vec.zero
  leftMouse(onBtnDown = {
    m =>
      val traces = tracer.tracesNearCoord(m, -1 to 1, condition = {
        trace => trace.location.dist(m) < 10
      })
      if (!traces.isEmpty) {
        polygons += tracer.tracesList.map(_.location).toList
        tracer.removeAllTraces()
        last_vertice = Vec.zero
      } else {
        tracer.addTrace(m, Trace())
        last_vertice = m
        selected_polygon = -1
      }
  })

  rightMouse(onBtnDown = {
    m =>
      if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
        polygons -= polygons(selected_polygon)
        selected_polygon = -1
      }
  })

  private var selected_polygon = -1
  mouseWheelUp(onWheelUp = {
    m =>
      if (last_vertice == Vec.zero) {
        selected_polygon += 1
        if (selected_polygon >= polygons.length) selected_polygon = 0
      }
  })
  mouseWheelDown(onWheelDown = {
    m =>
      if (last_vertice == Vec.zero) {
        selected_polygon -= 1
        if (selected_polygon < 0) selected_polygon = polygons.length - 1
      }
  })

  private def mkVecString(vec: Vec) = {
    "Vec(" + vec.ix + ", " + vec.iy + ")"
  }

  key(KEY_P, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      var first = true
      val b = new StringBuilder
      for (coord <- polygons(selected_polygon)) {
        if (first) {
          b append mkVecString(coord)
          first = false
        } else {
          b append ", "
          b append mkVecString(coord)
        }
      }
      println(b.toString())
    }
  })

  render {
    drawTraceGrid(tracer, DARK_GRAY)
    if (last_vertice == Vec.zero) {
      for {
        (polygon, index) <- polygons.zipWithIndex
        polygon_color = if (index == selected_polygon) RED else GREEN
      } {
        drawPolygon(polygon, polygon_color)
      }
    } else {
      for (polygon <- polygons) {
        drawPolygon(polygon, GREEN)
      }
    }

    val trace_coords = tracer.tracesList.map(_.location).toList
    drawPolygon(trace_coords, RED)
    for (coord <- trace_coords) {
      drawCircle(coord, 10, RED)
    }

    if (last_vertice != Vec.zero) {
      drawLine(last_vertice, mouseCoord, RED)
    }
  }

  interface {
    print(mouseCoord, 20, 20, GREEN)
  }
}
