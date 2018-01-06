package com.github.dunnololda.scageprojects.runnegun

import com.github.dunnololda.scage.ScageLib._

import scala.collection.mutable.ArrayBuffer

trait GameObject extends TraceTrait {
  type ChangerType = GameObject
}

object Runnegun extends ScageScreenApp("Runnegun") {
  val tracer = new CoordTracer[GameObject](solid_edges = false)

  val bullet_radius = property("bullet.radius", 2)
  val player_radius = property("player.radius", 10)
  val enemy_radius = property("enemy.radius", 10)
  val num_enemies = property("enemy.amount", 5)

  private var score = 0

  private val enemies = ArrayBuffer[Enemy]()

  init {
    score = 0
    for(i <- 1 to num_enemies) enemies += new Enemy()
  }

  action {
    if(!Player.isAlive) pause()
    val dead_enemies = enemies.filter(!_.isAlive)
    score += dead_enemies.length
    enemies --= dead_enemies
    if(enemies.size < math.random*num_enemies) enemies += new Enemy()
  }

  clear {
    enemies.clear()
  }

  keyIgnorePause(KEY_SPACE, onKeyDown = {switchPause()})
  keyIgnorePause(KEY_Q, onKeyDown = if(keyPressed(KEY_LCONTROL) || keyPressed(KEY_RCONTROL)) stopApp())

  keyOnPause(KEY_Y, onKeyDown = {
    restart()
    pauseOff()
  })
  keyOnPause(KEY_N, onKeyDown = stop())

  backgroundColor = WHITE
  interface {
    print(fps, 10, windowHeight-20, BLACK)
    print("SCORE: "+score, 10, windowHeight-40, BLACK)
    if(onPause) {
      if(!Player.isAlive) printCentered("PLAY AGAIN? (Y/N)", windowWidth/2, windowHeight/2, BLACK)
      else printCentered("PAUSE (Press Space to continue)", windowWidth/2, windowHeight/2, BLACK)
    }
  }
}