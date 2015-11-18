package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

case class EngineData(coord:Vec, force_dir:Vec)

object ShipDesigner extends ScageScreenApp("Ship Designer", 640, 480) {
  private val points = collection.mutable.ArrayBuffer[Vec](
    //Vec(-10.0, 70.74072), Vec(-10.0, 10.740723), Vec(-50.0, -9.259277), Vec(-10.0, -9.259277), Vec(-10.0, -69.25928), Vec(10.0, -69.25928), Vec(10.0, -9.259277), Vec(50.0, -9.259277), Vec(10.0, 10.740723), Vec(10.0, 70.74072), Vec(-10.0, 70.74072)
  ).map(p => p + windowCenter)
  private var selected_point = 0

  private val engines = collection.mutable.ArrayBuffer[EngineData]()
  private val engines_mapping = collection.mutable.HashMap[Int, Int]()
  private var selected_engine = 0

  private var mass_center = windowCenter

  private var mode = 0  // 0 - points, 1 - engines up, 2 - engines down, 3 - engines right, 4 - engines left, 5 - mass center set

  private def nearestDot(x:Float, a:Float, h:Float):Float = {
    val x1 = a + ((x - a)/h).toInt*h
    val x2 = x1+h
    if(x - x1 < x2 - x) x1 else x2
  }

  private def engineMappingStr(code:Int):(String, String, String) = {
    code match {
      case KEY_NUMPAD9 => ("9", "nine",  "KEY_NUMPAD9")
      case KEY_NUMPAD8 => ("8", "eight", "KEY_NUMPAD8")
      case KEY_NUMPAD7 => ("7", "seven", "KEY_NUMPAD7")
      case KEY_NUMPAD6 => ("6", "six",   "KEY_NUMPAD6")
      case KEY_NUMPAD4 => ("4", "four",  "KEY_NUMPAD4")
      case KEY_NUMPAD3 => ("3", "three", "KEY_NUMPAD3")
      case KEY_NUMPAD2 => ("2", "two",   "KEY_NUMPAD2")
      case KEY_NUMPAD1 => ("1", "one",   "KEY_NUMPAD1")
      case _           => ("0", "unknown", "UNKNOWN")
    }
  }

  /*def polygonCentroid(vertices:Seq[Vec]):Vec = {
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
  }*/

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  key(KEY_E,     onKeyDown = {if(mode == 0) mode = 1 else mode = 0})
  key(KEY_UP,    onKeyDown = {if(mode != 1) mode = 1 else mode = 0})
  key(KEY_DOWN,  onKeyDown = {if(mode != 2) mode = 2 else mode = 0})
  key(KEY_RIGHT, onKeyDown = {if(mode != 3) mode = 3 else mode = 0})
  key(KEY_LEFT,  onKeyDown = {if(mode != 4) mode = 4 else mode = 0})
  key(KEY_M,     onKeyDown = {if(mode != 5) mode = 5 else mode = 0})
  
  key(KEY_NUMPAD8, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD8)
  key(KEY_NUMPAD2, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD2)
  
  key(KEY_NUMPAD6, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD6)
  key(KEY_NUMPAD4, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD4)
  
  key(KEY_NUMPAD9, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD9)
  key(KEY_NUMPAD7, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD7)
  
  key(KEY_NUMPAD3, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD3)
  key(KEY_NUMPAD1, onKeyDown = engines_mapping(selected_engine) = KEY_NUMPAD1)

  key(KEY_P, onKeyDown = {
    if(points.length > 1) {
      println("====================================")
      //val mass_center = polygonCentroid(points)
      println(points.map(p => {
        val pp = p - mass_center
        s"  DVec(${pp.x}, ${pp.y})"
      }).mkString("val points:List[DVec] = List(\n", ",\n", "\n)"))
      println()
      engines.zipWithIndex.foreach {case (e, idx) =>
        val mapping = engines_mapping.getOrElse(idx, 0)
        val (index, val_name, _) = engineMappingStr(mapping)
        val position = e.coord - mass_center
        println(s"""val $val_name = Engine("$index", position = Vec(${position.x}, ${position.y}), force_dir = Vec(${e.force_dir.x}, ${e.force_dir.y}), max_power = 10, default_power_percent = 1, this)""")
      }
      println()
      println(engines.zipWithIndex.map {case (e, idx) =>
        val mapping = engines_mapping.getOrElse(idx, 0)
        val (_, val_name, _) = engineMappingStr(mapping)
        val_name
      }.mkString(s"val engines = List(", ", ", ")"))
      println()
      println(engines.zipWithIndex.map {
        case (e, idx) =>
          val mapping = engines_mapping.getOrElse(idx, 0)
          val (_, val_name, key_name) = engineMappingStr(mapping)
          s"  $key_name -> $val_name"
      }.mkString("val engines_mapping = Map(\n", ",\n", "\n)"))
      println()
      println("val draw_points = points :+ points.head")
      println("====================================")
    }
  })

  leftMouse(onBtnDown = m => {
    mode match {
      case 0 =>
        val ssm = absCoord(m)
        if(points.forall(p => p.dist2(ssm) > 1)) {
          val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1).toInt,
                       nearestDot(ssm.y, -windowHeight/2, 1).toInt)
          points.insert(selected_point, sm)
        }
      case 1 => // engines up
        val ssm = absCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1) + 0.5f,
                     nearestDot(ssm.y, -windowHeight/2, 1))
        engines += EngineData(sm, Vec(0, -1))
        selected_engine = engines.length-1
      case 2 => // engines down
        val ssm = absCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1) + 0.5f,
                     nearestDot(ssm.y, -windowHeight/2, 1))
        engines += EngineData(sm, Vec(0, 1))
        selected_engine = engines.length-1
      case 3 => // engines right
        val ssm = absCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1),
                     nearestDot(ssm.y, -windowHeight/2, 1) + 0.5f)
        engines += EngineData(sm, Vec(-1, 0))
        selected_engine = engines.length-1
      case 4 => // engines left
        val ssm = absCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1),
                     nearestDot(ssm.y, -windowHeight/2, 1) + 0.5f)
        engines += EngineData(sm, Vec(1, 0))
        selected_engine = engines.length-1
      case 5 => // mass center set
        val ssm = absCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1)+0.5f,
                     nearestDot(ssm.y, -windowHeight/2, 1)+0.5f)
        mass_center = sm
      case _ =>
    }
  }, onBtnUp = m => {
    mode match {
      case 0 =>
        val ssm = absCoord(m)
        val sm = Vec(nearestDot(ssm.x, -windowWidth/2, 1),
                     nearestDot(ssm.y, -windowHeight/2, 1))
        points(selected_point) = sm
      case _ =>
    }
  })

  rightMouse(onBtnDown = m => {
    mode match {
      case 0 =>
        if(points.length > 0) {
          points.remove(selected_point)
        }
        if(points.length > 0) selected_point = points.length-1
        else selected_point = 0
      case x if x > 0 && x < 5 =>
        if(engines.length > 0) {
          engines.remove(selected_engine)
        }
        if(engines.length > 0) selected_engine = engines.length-1
        else selected_engine = 0
      case _ =>
    }
  })

  mouseWheelDown(onWheelDown = m => {
    mode match {
      case 0 =>
        selected_point -= 1
        if(selected_point < 0) selected_point = points.length-1
      case x if x > 0 && x < 5 =>
        selected_engine -= 1
        if(selected_engine < 0) selected_engine = engines.length-1
      case _ =>
    }
  })

  mouseWheelUp(onWheelUp = m => {
    mode match {
      case 0 =>
        selected_point += 1
        if(selected_point >= points.length) selected_point = 0
      case x if x > 0 && x < 5 =>
        selected_engine += 1
        if(selected_engine >= engines.length) selected_engine = 0
      case _ =>
    }
  })

  leftMouseDrag(onDrag = m => {
    mode match {
      case 0 =>
        val ssm = absCoord(m)
        points.zipWithIndex.find(p => p._1.dist2(ssm) < 1).foreach(p => {
          selected_point = p._2
          points(selected_point) = ssm
        })
      case _ =>
    }
  })

  globalScale = 20

  render {
    (0 to windowWidth  by 1).foreach(x => drawLine(Vec(x, 0), Vec(x, windowHeight), DARK_GRAY))
    (0 to windowHeight by 1).foreach(y => drawLine(Vec(0, y), Vec(windowWidth,  y), DARK_GRAY))

    if(points.length > 0) {
      points.zipWithIndex.foreach(p => drawFilledCircle(p._1, 0.3f, if(p._2 == selected_point) RED else WHITE))
      drawSlidingLines(points.:+(points.head), WHITE)
    }

    /*if(points.length > 1) {*/
    //val mass_center = polygonCentroid(points)
    drawCircle(mass_center, 0.3f, GREEN)
    /*}*/

    if(engines.length > 0) {
      engines.zipWithIndex.foreach {
        case (EngineData(coord, force_dir), idx) =>
          force_dir match {
            case Vec(0, -1) => drawRectCentered(coord + Vec(0, 0.125f),  0.5f,  0.25f, if(idx == selected_engine) RED else WHITE)
            case Vec(0, 1)  => drawRectCentered(coord + Vec(0, -0.125f), 0.5f,  0.25f, if(idx == selected_engine) RED else WHITE)
            case Vec(-1, 0) => drawRectCentered(coord + Vec(0.125f, 0),  0.25f, 0.5f,  if(idx == selected_engine) RED else WHITE)
            case Vec(1, 0)  => drawRectCentered(coord + Vec(-0.125f, 0), 0.25f, 0.5f,  if(idx == selected_engine) RED else WHITE)
            case _ =>
          }
          print(engineMappingStr(engines_mapping.getOrElse(idx, 0))._1, coord, max_font_size/globalScale, if(idx == selected_engine) RED else WHITE)
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
      case 5 => "mass center"
      case _ => ""
    }
    print(status, 20, 20, WHITE)
  }
}
