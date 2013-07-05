package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer
import com.github.dunnololda.cli.Cli

object MapCreator extends ScageScreenApp(s"Simple Shooter Map Creator v$appVersion", default_window_width, default_window_height) with Cli {
  programDescription = s"Simple Shooter Map Creator $appVersion"
  commandLineArgsAndParse(("m", "map", "optional map file name to edit or the name for new map. Default: map.ss", true, false))
  private val map_name = property("map", "map.ss")

  private var loaded_map = loadMap(map_name)

  private var from:Option[Vec] = None
  private val walls = ArrayBuffer[Wall](loaded_map.walls:_*)
  private var walls_counter = 0

  private val safe_zones = ArrayBuffer[List[Vec]](loaded_map.safe_zones:_*)
  private val new_safe_zone = ArrayBuffer[Vec]()
  private var safe_zones_counter = 0

  private val control_points = ArrayBuffer[(Int, ControlPoint)](loaded_map.control_points.toSeq:_*)
  private val new_control_point_area = ArrayBuffer[Vec]()
  private var control_points_counter = 0

  private var mode = 0  // 0 - walls, 1 - safe zones, 2 - chance modificators


  private var _center:Vec = Vec.zero

  private var _map_changed = false
  private def isMapChanged:Boolean = _map_changed

  key(KEY_Z, onKeyDown = {
    mode match {
      case 0 =>
        mode = 1
        new_safe_zone.clear()
      case 1 =>
        mode = 2
        new_control_point_area.clear()
      case 2 =>
        mode = 0
      case _ =>
        from = None
    }
  })

  private var dir = Vec.zero
  key(KEY_W, 100, onKeyDown = dir += Vec(0, 1))
  key(KEY_A, 100, onKeyDown = dir += Vec(-1, 0))
  key(KEY_S, 100, onKeyDown = dir += Vec(0, -1))
  key(KEY_D, 100, onKeyDown = dir += Vec(1, 0))

  key(KEY_F1, onKeyDown = {
    walls.clear()
    walls ++= loaded_map.walls
    walls_counter = 0
    safe_zones.clear()
    safe_zones ++= loaded_map.safe_zones
    safe_zones_counter = 0
    control_points.clear()
    control_points ++= loaded_map.control_points.toSeq
    control_points_counter = 0
    _map_changed = false
  })
  key(KEY_F5, onKeyDown = {
    if(isMapChanged) {
      saveMap(map_name, walls, safe_zones, control_points)
      loaded_map = GameMap(walls.toList, safe_zones.toList, control_points.toMap)
      _map_changed = false
    }
  })

  key(KEY_LEFT, 100, onKeyDown = {
    mode match {
      case 0 =>
        if(walls.length > 0) {
          walls_counter -= 1
          if(walls_counter < 0) walls_counter = walls.length-1
        }
      case 1 =>
        if(safe_zones.length > 0) {
          safe_zones_counter -= 1
          if(safe_zones_counter < 0) safe_zones_counter = safe_zones.length-1
        }
      case 2 =>
        if(control_points.length > 0) {
          control_points_counter -= 1
          if(control_points_counter < 0) control_points_counter = control_points.length-1
        }
      case _ =>
    }
  })

  key(KEY_RIGHT, 100, onKeyDown = {
    mode match {
      case 0 =>
        if(walls.length > 0) {
          walls_counter += 1
          if(walls_counter >= walls.length) walls_counter = 0
        }
      case 1 =>
        if(safe_zones.length > 0) {
          safe_zones_counter += 1
          if(safe_zones_counter >= safe_zones.length) safe_zones_counter = 0
        }
      case 2 =>
        if(control_points.length > 0) {
          control_points_counter += 1
          if(control_points_counter >= control_points.length) control_points_counter = 0
        }
      case _ =>
    }
  })

  private def nearestDot(x:Float, a:Float, h:Float):Float = {
    val x1 = a + ((x - a)/h).toInt*h
    val x2 = x1+h
    if(x - x1 < x2 - x) x1 else x2
  }

  leftMouse(onBtnDown = m => {
    val ssm = scaledCoord(m)
    val sm = Vec(nearestDot(ssm.x, -map_width/2, human_size*2),
                 nearestDot(ssm.y, -map_height/2, human_size*2))
    if(isCoordInsideMapBorders(sm)) {
      mode match {
        case 0 =>
          from match {
            case Some(f) =>
              walls += Wall(f, sm)
              //bresenham(f, sm, -map_width/2, -map_height/2, human_size*2, true)
              _map_changed = true
              from = None
            case None => from = Some(sm)
          }
        case 1 =>
          if(new_safe_zone.length > 0) {
            if(sm.dist2(new_safe_zone.head) < human_size*human_size) {
              if(new_safe_zone.length < 3) {
                new_safe_zone.clear()
              } else {
                new_safe_zone += new_safe_zone.head
                safe_zones += new_safe_zone.toList
                new_safe_zone.clear()
                _map_changed = true
              }
            } else new_safe_zone += sm
          } else new_safe_zone += sm
        case 2 =>
          if(new_control_point_area.length > 0) {
            if(sm.dist2(new_control_point_area.head) < human_size*human_size) {
              if(new_control_point_area.length < 3) {
                new_control_point_area.clear()
              } else {
                new_control_point_area += new_control_point_area.head
                val number = control_points.length
                control_points += (number -> ControlPoint(number, None, 0l, new_control_point_area.toList))
                new_control_point_area.clear()
                _map_changed = true
              }
            } else new_control_point_area += sm
          } else new_control_point_area += sm
        case _ =>
      }
    }
  })

  rightMouse(onBtnDown = m => {
    mode match {
      case 0 =>
        if(walls.length > 0 && walls_counter >= 0 && walls_counter < walls.length) {
          walls.remove(walls_counter)
          _map_changed = true
          if(walls.length > 0) walls_counter -= 1
          else walls_counter = 0
        }
      case 1 =>
        if(safe_zones.length > 0 && safe_zones_counter >= 0 && safe_zones_counter < safe_zones.length) {
          safe_zones.remove(safe_zones_counter)
          _map_changed = true
          if(safe_zones.length > 0) safe_zones_counter -= 1
          else safe_zones_counter = 0
        }
      case 2 =>
        if(control_points.length > 0 && control_points_counter >= 0 && control_points_counter < control_points.length) {
          control_points.remove(control_points_counter)
          _map_changed = true
          if(control_points.length > 0) control_points_counter -= 1
          else control_points_counter = 0
        }
      case _ =>
    }
  })

  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 0.1f) {
      if(globalScale > 1) globalScale -= 1
      else globalScale -= 0.1f
    }
  })

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 3) globalScale += 1
  })

  action {
    if(dir.notZero) {
      val new_center = _center + dir.n*human_size
      if(isCoordInsideMapBorders(new_center)) _center = new_center
      dir = Vec.zero
    }
  }

  center = _center

  render {
    drawSlidingLines(map_edges, DARK_GRAY)
    (-map_width/2 to map_width/2 by human_size.toInt*2).foreach(x => drawLine(Vec(x, -map_height/2), Vec(x, map_height/2), DARK_GRAY))
    (-map_height/2 to map_height/2 by human_size.toInt*2).foreach(y => drawLine(Vec(-map_width/2, y), Vec(map_width/2, y), DARK_GRAY))
    drawFilledCircle(_center, 2, GREEN)
    walls.zipWithIndex.foreach {
      case (w, i) =>
        if(mode == 0 && i == walls_counter) drawLine(w.from, w.to, RED)
        else drawLine(w.from, w.to, WHITE)
        /*drawCircle(w.from, near_wall_area, GRAY)
        drawCircle(w.to, near_wall_area, GRAY)*/
        /*val cell_size = human_size*2
        bresenham(w.from, w.to, -map_width/2, -map_height/2, cell_size).foreach( c => {
          val x = -map_width/2 + c._1*cell_size + cell_size/2
          val y = -map_height/2 + c._2*cell_size+ cell_size/2
          drawRectCentered(Vec(x, y), cell_size, cell_size, GREEN)
        })*/
    }
    safe_zones.zipWithIndex.foreach {
      case (sz, idx) =>
        if(mode == 1 && idx == safe_zones_counter) drawSlidingLines(sz, RED)
        else drawSlidingLines(sz, GREEN)
    }
    control_points.zipWithIndex.foreach {
      case ((number, cm), idx) =>
        if(mode == 2 && idx == control_points_counter) {
          drawSlidingLines(cm.area, RED)
        } else {
          drawSlidingLines(cm.area, GRAY)
        }
    }
    mode match {
      case 0 =>
        from match {
          case Some(f) =>
            drawFilledCircle(f, 2, WHITE)
            drawLine(f, scaledCoord(mouseCoord), WHITE)
          case None =>
        }
      case 1 =>
        drawSlidingLines(new_safe_zone.toList ::: scaledCoord(mouseCoord) :: Nil, GREEN)
      case 2 =>
        drawSlidingLines(new_control_point_area.toList ::: scaledCoord(mouseCoord) :: Nil, GRAY)
      case _ =>
    }
  }

  interface {
    print(fps, 20, windowHeight-20, WHITE)
    val mode_str = mode match {
      case 0 => "Добавляем стены"
      case 1 => "Добавляем мертвяк"
      case 2 => "Добавляем контрольную точку"
      case _ =>
    }
    print(mode_str, 20, 80, GREEN)
    print(f"Центр: x = ${_center.x/10f}%.2f м, y = ${_center.y/10f}%.2f м", 20, 50, GREEN)
    val s = if(isMapChanged) {
      s"Карта: $map_name. Сохранить: F5. Восстановить сохраненную карту: F1. Добавление стен/мертвяка: Z"
    } else s"Карта: $map_name. Сохранено"
    print(s, 20, 20, GREEN)
  }

  dispose {
    if(isMapChanged) saveMap(map_name, walls, safe_zones, control_points.toList)
  }
}
