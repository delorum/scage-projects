package com.github.dunnololda.scageprojects.scarcanoid

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.scarcanoid.levels.LevelMap1

object Scaranoid extends ScageScreenApp("Scaranoid") {
  val physics = ScagePhysics()
  physics.addPhysicals(PlayerBall, PlayerPlatform)
  action {
    physics.step()
  }

  var count = 0
  var bonus = 1
  private var current_level = 1
  val max_level = property("level.max", 1)
  init {
    bonus = 0
    current_level match {
      case 1 => Level.loadMap(LevelMap1)
      case _ => Level.loadMap(LevelMap1)
    }
  }

  interface {
    print(count, 5, windowHeight-20, WHITE)
    print("+"+bonus, 5, windowHeight-40, WHITE)
    print(physics.world.getBodies.size(), 5, windowHeight-60, WHITE)

    if(onPause) {
      if(Level.winCondition) print(xml("game.win"), windowWidth/2, windowHeight/2, WHITE)
      else print(xml("game.lose"), windowWidth/2, windowHeight/2, WHITE)
      print(xml("game.playagain"), windowWidth/2, windowHeight/2-20, WHITE)
    }
  }

  keyIgnorePause(KEY_Y, onKeyDown = if(onPause) {
    if(Level.winCondition) {
      if(current_level == max_level) current_level = 1
      else current_level += 1
    } else {
      count = 0
      current_level = 1
    }

    init()
    pauseOff()
  })
  keyIgnorePause(KEY_N, onKeyDown = if(onPause) stop())
  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp())

  new ScageScreen("Help Screen") {
    key(KEY_SPACE, onKeyDown = stop())

    interface {
      print(xml("helpscreen.helpmessage"), 10, windowHeight-20, WHITE)
    }
  }.run()
}