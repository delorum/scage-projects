package com.github.dunnololda.scageprojects.blases.levelbuilder

import java.io.FileOutputStream

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

case class LevelElement(element_type:String, coords:List[Vec]) {
  private def mkrVecString(vec: Vec) = {
    "rVec(" + vec.ix + ", " + vec.iy + ")"
  }

  override def toString:String = {
    var first = true
    val b = new StringBuilder
    b append element_type+": "
    for (coord <- coords) {
      if (first) {
        b append mkrVecString(coord)
        first = false
      } else {
        b append ", "
        b append mkrVecString(coord)
      }
    }

    b.toString()
  }
}

object LevelBuilder2 extends ScageScreenApp("Level Builder 2") {
  private val polygons = ArrayBuffer[LevelElement]()
  val tracer = CoordTracer()

  def avg(points:List[Vec]):Vec = Vec(points.map(_.x).sum/points.length, points.map(_.y).sum/points.length)
  
  private var last_vertice = Vec.zero
  leftMouse(onBtnDown = {
    m =>
      val traces = tracer.tracesNearCoord(m, -1 to 1, condition = {
        trace => trace.location.dist(m) < 10
      })
      if (traces.nonEmpty) {
        polygons += LevelElement("Obstacle", tracer.tracesList.map(_.location))
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

  key(KEY_F1, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Spikes", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F2, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Speed", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F3, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Start", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F4, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Finish", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F5, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Obstacle", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F6, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Star", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F7, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Moving Spikes", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F8, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Moving Obstacle", polygons(selected_polygon).coords)
    }
  })
  key(KEY_F9, onKeyDown = {
    if (last_vertice == Vec.zero && selected_polygon >= 0 && selected_polygon < polygons.length) {
      polygons(selected_polygon) = LevelElement("Sparkles", polygons(selected_polygon).coords)
    }
  })

  render {
    drawTraceGrid(tracer, DARK_GRAY)
    if (last_vertice == Vec.zero) {
      for {
        (LevelElement(polygon_type, polygon_points), index) <- polygons.zipWithIndex
        polygon_color = if (index == selected_polygon) RED else GREEN
      } {
        drawPolygon(polygon_points, polygon_color)
        print(polygon_type, avg(polygon_points), polygon_color)
      }
    } else {
      for (LevelElement(polygon_type, polygon_points) <- polygons) {
        drawPolygon(polygon_points, GREEN)
        print(polygon_type, avg(polygon_points) + Vec(-50, 0), GREEN)
      }
    }

    val trace_coords = tracer.tracesList.map(_.location)
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
  
  dispose {
    val fos = new FileOutputStream("level.txt")
    for(level_element <- polygons) {
      fos.write((level_element+"\n").getBytes)
    }
    fos.flush()
    fos.close()
  }
}
