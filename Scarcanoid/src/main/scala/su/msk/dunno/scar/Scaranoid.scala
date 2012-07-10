package su.msk.dunno.scar

import levels.LevelMap1
import net.scage.support.physics.ScagePhysics
import net.scage.{ScageLib, ScageScreenApp, ScageScreen}

object Scaranoid extends ScageScreenApp("Scaranoid") with ScageLib {
  val physics = ScagePhysics(PlayerBall, PlayerPlatform)
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
    print(count, 5, window_height-20, WHITE)
    print("+"+bonus, 5, window_height-40, WHITE)
    print(physics.world.getBodies.size(), 5, window_height-60, WHITE)

    if(onPause) {
      if(Level.winCondition) print(xml("game.win"), window_width/2, window_height/2, WHITE)
      else print(xml("game.lose"), window_width/2, window_height/2, WHITE)
      print(xml("game.playagain"), window_width/2, window_height/2-20, WHITE)
    }
  }

  key(KEY_Y, onKeyDown = if(onPause) {
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
  key(KEY_N, onKeyDown = if(onPause) stop())

  new ScageScreen("Help Screen") {
    key(KEY_SPACE, onKeyDown = stop())

    interface {
      print(xml("helpscreen.helpmessage"), 10, window_height-20, WHITE)
    }
  }.run()
}