package com.github.dunnololda.scageprojects.simpleshooter

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer
import java.io.FileOutputStream
import com.github.dunnololda.cli.Cli

object MapCreator extends ScageScreenApp(s"Simple Shooter Map Creator v$appVersion", map_width, map_height) with Cli {
  programDescription = s"Simple Shooter Map Creator $appVersion"
  commandLineArgsAndParse(("m", "map", "optional map file name to edit or the name for new map. Default: map.ss", true, false))
  val map_name = property("map", "map.ss")

  private val loaded_map = loadMap(map_name)
  private val walls = ArrayBuffer[Wall](loaded_map:_*)
  private var wall_counter = 0
  private var from:Option[Vec] = None

  render {
    (0 to map_width by body_radius*2).foreach(x => drawLine(Vec(x, 0), Vec(x, map_height), DARK_GRAY))
    (0 to map_height by body_radius*2).foreach(y => drawLine(Vec(0, y), Vec(map_width, y), DARK_GRAY))
    walls.zipWithIndex.foreach {
      case (w, i) =>
        if(i == wall_counter) drawLine(w.from, w.to, RED)
        else drawLine(w.from, w.to, WHITE)
    }
    from match {
      case Some(f) =>
        drawFilledCircle(f, 2, WHITE)
        drawLine(f, mouseCoord, WHITE)
      case None =>
    }
  }

  mouseWheelDown(m => {
    if(walls.length > 0) {
      wall_counter -= 1
      if(wall_counter < 0) wall_counter = walls.length-1
    }
  })

  mouseWheelUp(m => {
    if(walls.length > 0) {
      wall_counter += 1
      if(wall_counter >= walls.length) wall_counter = 0
    }
  })

  rightMouse(onBtnDown = m => {
    if(walls.length > 0 && wall_counter >= 0 && wall_counter < walls.length) {
      walls.remove(wall_counter)
      if(walls.length > 0) wall_counter -= 1
      else wall_counter = 0
    }
  })

  leftMouse(onBtnDown = m => {
    from match {
      case Some(f) =>
        walls += Wall(f, m)
        from = None
      case None => from = Some(m)
    }
  })

  key(KEY_F1, onKeyDown = {
    walls.clear()
    walls ++= loaded_map
  })

  interface {
    print("F1 - Restore Initial Map", 20, 20, GREEN)
  }

  dispose {
    if(walls.toList != loaded_map) {
      val fos = new FileOutputStream(map_name)
      walls.foreach(w => {
        fos.write(s"${w.from.x} ${w.from.y} ${w.to.x} ${w.to.y}\n".getBytes)
      })
      fos.close()
    }
  }
}
