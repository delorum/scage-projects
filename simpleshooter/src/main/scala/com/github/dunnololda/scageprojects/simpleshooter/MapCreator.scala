package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer
import java.io.FileOutputStream
import com.github.dunnololda.cli.Cli

object MapCreator extends ScageScreenApp(s"Simple Shooter Map Creator v$appVersion", map_width, map_height) with Cli {
  programDescription = s"Simple Shooter Map Creator $appVersion"
  commandLineArgsAndParse(("m", "map", "optional map file name to edit or the name for new map. Default: map.ss", true, false))
  private val map_name = property("map", "map.ss")

  private var loaded_map = loadMap(map_name)

  private val walls = ArrayBuffer[Wall](loaded_map.walls:_*)
  private var walls_counter = 0

  private val safe_zones = ArrayBuffer[List[Vec]](loaded_map.safe_zones:_*)
  private val new_safe_zone = ArrayBuffer[Vec]()
  private var safe_zones_counter = 0

  private var safe_zone_creation_mode = false
  private var from:Option[Vec] = None
  private var _center:Vec = windowCenter

  private def isMapChanged:Boolean = {
    walls.toList != loaded_map.walls || safe_zones.toList != loaded_map.safe_zones
  }

  key(KEY_Z, onKeyDown = {
    safe_zone_creation_mode = !safe_zone_creation_mode
    new_safe_zone.clear()
  })
  key(KEY_W, 100, onKeyDown = _center += Vec(0, body_radius))
  key(KEY_A, 100, onKeyDown = _center += Vec(-body_radius, 0))
  key(KEY_S, 100, onKeyDown = _center += Vec(0, -body_radius))
  key(KEY_D, 100, onKeyDown = _center += Vec(body_radius, 0))

  key(KEY_F1, onKeyDown = {
    walls.clear()
    walls ++= loaded_map.walls
    safe_zones.clear()
    safe_zones ++= loaded_map.safe_zones
  })
  key(KEY_F5, onKeyDown = {
    if(isMapChanged) {
      saveMap(map_name, walls, safe_zones)
      loaded_map = GameMap(walls.toList, safe_zones.toList)
    }
  })

  leftMouse(onBtnDown = m => {
    val sm = scaledCoord(m)
    if(!safe_zone_creation_mode) {
      from match {
        case Some(f) =>
          walls += Wall(f, sm)
          from = None
        case None => from = Some(sm)
      }
    } else {
      if(new_safe_zone.length > 0) {
        if(sm.dist2(new_safe_zone.head) < body_radius*body_radius) {
          if(new_safe_zone.length < 3) {
            new_safe_zone.clear()
          } else {
            new_safe_zone += new_safe_zone.head
            safe_zones += new_safe_zone.toList
            new_safe_zone.clear()
          }
        } else new_safe_zone += sm
      } else new_safe_zone += sm
    }
  })

  rightMouse(onBtnDown = m => {
    if(!safe_zone_creation_mode) {
      if(walls.length > 0 && walls_counter >= 0 && walls_counter < walls.length) {
        walls.remove(walls_counter)
        if(walls.length > 0) walls_counter -= 1
        else walls_counter = 0
      }
    } else {
      if(safe_zones.length > 0 && safe_zones_counter >= 0 && safe_zones_counter < safe_zones.length) {
        safe_zones.remove(safe_zones_counter)
        if(safe_zones.length > 0) safe_zones_counter -= 1
        else safe_zones_counter = 0
      }
    }
  })

  mouseWheelDown(m => {
    if(!safe_zone_creation_mode) {
      if(walls.length > 0) {
        walls_counter -= 1
        if(walls_counter < 0) walls_counter = walls.length-1
      }
    } else {
      if(safe_zones.length > 0) {
        safe_zones_counter -= 1
        if(safe_zones_counter < 0) safe_zones_counter = safe_zones.length-1
      }
    }
  })

  mouseWheelUp(m => {
    if(!safe_zone_creation_mode) {
      if(walls.length > 0) {
        walls_counter += 1
        if(walls_counter >= walls.length) walls_counter = 0
      }
    } else {
      if(safe_zones.length > 0) {
        safe_zones_counter += 1
        if(safe_zones_counter >= walls.length) safe_zones_counter = 0
      }
    }
  })

  center = _center

  render {
    (0 to map_width by body_radius*2).foreach(x => drawLine(Vec(x, 0), Vec(x, map_height), DARK_GRAY))
    (0 to map_height by body_radius*2).foreach(y => drawLine(Vec(0, y), Vec(map_width, y), DARK_GRAY))
    drawFilledCircle(_center, 2, GREEN)
    walls.zipWithIndex.foreach {
      case (w, i) =>
        if(!safe_zone_creation_mode && i == walls_counter) drawLine(w.from, w.to, RED)
        else drawLine(w.from, w.to, WHITE)
    }
    safe_zones.zipWithIndex.foreach {
      case (sz, idx) =>
        if(safe_zone_creation_mode && idx == safe_zones_counter) drawSlidingLines(sz, RED)
        else drawSlidingLines(sz, GREEN)
    }
    if(!safe_zone_creation_mode) {
      from match {
        case Some(f) =>
          drawFilledCircle(f, 2, WHITE)
          drawLine(f, scaledCoord(mouseCoord), WHITE)
        case None =>
      }
    } else {
      drawSlidingLines(new_safe_zone.toList ::: scaledCoord(mouseCoord) :: Nil, GREEN)
    }
  }

  interface {
    print(fps, 20, windowHeight-20, WHITE)
    print(if(safe_zone_creation_mode) "Добавляем мертвяк" else "Добавляем стены", 20, 80, GREEN)
    print(f"Центр: x = ${_center.x/10f}%.2f м, y = ${_center.y/10f}%.2f м", 20, 50, GREEN)
    val s = if(isMapChanged) {
      s"Карта: $map_name. Сохранить: F5. Восстановить сохраненную карту: F1. Добавление стен/мертвяка: Z"
    } else s"Карта: $map_name. Сохранено"
    print(s, 20, 20, GREEN)
  }

  dispose {
    if(isMapChanged) saveMap(map_name, walls, safe_zones)
  }
}
