package net.scage.blases.ui

import net.scage.support.Vec
import net.scage.Screen
import net.scage.handlers.controller2.MultiController
import net.scage.blases.Relatives._
import net.scage.ScageLib._
import net.scage.blases.LevelSelector._
import net.scage.blases.{Blases, IntersectablePolygon, Level}

class LevelButton(level:Level,
                  level_num:Int,
                  coord:Vec,
                  screen: Screen with MultiController,
                  var visible:Boolean = true) extends IntersectablePolygon {
  val intersectableVertices = List(rVec(coord) + Vec(-20, 20),
                                   rVec(coord) + Vec(20, 20),
                                   rVec(coord) + Vec(20, -20),
                                   rVec(coord) + Vec(-20, -20))

  screen.interface {
    if(visible) {
      if(level.is_passed) currentColor = BLACK
      else currentColor = GRAY
      drawRectCentered(rVec(coord), 40, 40)
      print(level_num, rVec(coord) - Vec(5, 5))
    }
  }

  screen.leftMouseNoPause(onBtnDown = m => if(visible && level.is_passed && containsCoord(m)) {
    currentLevelNum = level_num
    screen.stop()
    MainMenu.runScreen(Blases)
  })
}
