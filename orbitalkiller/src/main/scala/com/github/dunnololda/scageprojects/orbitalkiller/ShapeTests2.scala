package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

case class EngineData(coord:Vec, force_dir:Vec)

object ShapeTests2 extends ScageScreenApp("Shape Tests 2", 640, 480) {
  private val points = collection.mutable.ArrayBuffer[Vec](
    Vec(-10.0, 70.74072), Vec(-10.0, 10.740723), Vec(-50.0, -9.259277), Vec(-10.0, -9.259277), Vec(-10.0, -69.25928), Vec(10.0, -69.25928), Vec(10.0, -9.259277), Vec(50.0, -9.259277), Vec(10.0, 10.740723), Vec(10.0, 70.74072), Vec(-10.0, 70.74072)
  ).map(p => p + windowCenter)
  private var selected_point = 0

  private val engines = collection.mutable.ArrayBuffer[EngineData]()
  private var selected_engine = 0

  private var mode = 0  // 0 - points, 1 - engines up, 2 - engines down, 3 - engines right, 4 - engines left

  private def nearestDot(x:Float, a:Float, h:Float):Float = {
    val x1 = a + ((x - a)/h).toInt*h
    val x2 = x1+h
    if(x - x1 < x2 - x) x1 else x2
  }

  key(KEY_E, onKeyDown = {if(mode == 0) mode = 1 else mode = 0})
  key(KEY_UP, onKeyDown = {if(mode != 1) mode = 1 else mode = 0})
  key(KEY_DOWN, onKeyDown = {if(mode != 2) mode = 2 else mode = 0})
  key(KEY_RIGHT, onKeyDown = {if(mode != 3) mode = 3 else mode = 0})
  key(KEY_LEFT, onKeyDown = {if(mode != 4) mode = 4 else mode = 0})

  key(KEY_P, onKeyDown = {
    if(points.length > 1) {
      val mass_center = polygonCentroid(points)
      println(points.map(p => {
        val pp = p - mass_center
        s"Vec(${pp.x}, ${pp.y})"
      }).mkString("List(", ", ", ")"))
      engines.zipWithIndex.foreach {case (e, idx) => {
        val position = e.coord - mass_center
        println(s"$idx: Engine(position = Vec(${position.x}, ${position.y}), force_dir = Vec(${e.force_dir.x}, ${e.force_dir.y}), max_power = 10, this)")
      }}
    }
  })

  leftMouse(onBtnDown = m => {
    mode match {
      case 0 =>
        val ssm = scaledCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 20),
                     nearestDot(ssm.y, -windowHeight/2, 20))
        points += sm
        selected_point = points.length-1
      case 1 => // engines up
        val ssm = scaledCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 20) + 10,
                     nearestDot(ssm.y, -windowHeight/2, 20))
        engines += EngineData(sm, Vec(0, -1))
        selected_engine = engines.length-1
      case 2 => // engines down
        val ssm = scaledCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 20) + 10,
                     nearestDot(ssm.y, -windowHeight/2, 20))
        engines += EngineData(sm, Vec(0, 1))
        selected_engine = engines.length-1
      case 3 => // engines right
        val ssm = scaledCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 20),
                     nearestDot(ssm.y, -windowHeight/2, 20) + 10)
        engines += EngineData(sm, Vec(-1, 0))
        selected_engine = engines.length-1
      case 4 => // engines left
        val ssm = scaledCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 20),
                     nearestDot(ssm.y, -windowHeight/2, 20) + 10)
        engines += EngineData(sm, Vec(1, 0))
        selected_engine = engines.length-1
      case _ =>
    }
  })

  rightMouse(onBtnDown = m => {
    mode match {
      case 0 =>
        points.remove(selected_point)
        if(points.length > 0) selected_point = points.length-1
        else selected_point = 0
      case _ =>
        engines.remove(selected_engine)
        if(engines.length > 0) selected_engine = engines.length-1
        else selected_engine = 0
    }
  })

  mouseWheelDown(onWheelDown = m => {
    mode match {
      case 0 =>
        selected_point -= 1
        if(selected_point < 0) selected_point = points.length-1
      case _ =>
        selected_engine -= 1
        if(selected_engine < 0) selected_engine = engines.length-1
    }
  })

  mouseWheelUp(onWheelUp = m => {
    mode match {
      case 0 =>
        selected_point += 1
        if(selected_point >= points.length) selected_point = 0
      case _ =>
        selected_engine += 1
        if(selected_engine >= engines.length) selected_engine = 0
    }
  })

  def polygonCentroid(vertices:Seq[Vec]):Vec = {
    val a = {
      vertices.sliding(2).map {
        case Seq(i, j) => i.x*j.y - i.y*j.x
      }.sum*0.5f
    }
    val x = vertices.sliding(2).map {
      case Seq(i, j) => (i.x + j.x)*(i.x*j.y - i.y*j.x)
    }.sum/6f/a

    val y = vertices.sliding(2).map {
      case Seq(i, j) => (i.y + j.y)*(i.x*j.y - i.y*j.x)
    }.sum/6f/a

    Vec(x, y)
  }

  render {
    (0 to windowWidth by 20).foreach(x => drawLine(Vec(x, 0), Vec(x, windowHeight), DARK_GRAY))
    (0 to windowHeight by 20).foreach(y => drawLine(Vec(0, y), Vec(windowWidth, y), DARK_GRAY))

    if(points.length > 0) {
      points.zipWithIndex.foreach(p => drawFilledCircle(p._1, 3, if(p._2 == selected_point) RED else WHITE))
      drawSlidingLines(points, WHITE)

      if(points.length > 1) {
        val mass_center = polygonCentroid(points)
        drawCircle(mass_center, 3, GREEN)
      }
    }

    if(engines.length > 0) {
      engines.zipWithIndex.foreach {
        case (EngineData(coord, force_dir), idx) =>
          force_dir match {
            case Vec(0, -1) => drawRectCentered(coord + Vec(0, 2.5f), 10, 5, if(idx == selected_engine) RED else WHITE)
            case Vec(0, 1) => drawRectCentered(coord + Vec(0, -2.5f), 10, 5, if(idx == selected_engine) RED else WHITE)
            case Vec(-1, 0) => drawRectCentered(coord + Vec(2.5f, 0), 5, 10, if(idx == selected_engine) RED else WHITE)
            case Vec(1, 0) => drawRectCentered(coord + Vec(-2.5f, 0), 5, 10, if(idx == selected_engine) RED else WHITE)
            case _ =>
          }
          print(idx, coord, if(idx == selected_engine) RED else WHITE)
      }
    }
  }

  interface {
    val status = mode match {
      case 0 => "points"
      case 1 => "engines up"
      case 2 => "engines down"
      case 3 => "engines right"
      case 4 => "engines left"
    }
    print(status, 20, 20, WHITE)
  }
}
