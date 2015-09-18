package com.github.dunnololda.scageprojects.runnegun

import com.github.dunnololda.scage.ScageLib._
import Runnegun._

object Player {
  private val trace = new GameObject {
    val state = new State("player")
    def changeState(changer:GameObject, s:State) {
      s.neededKeys {
        case ("hit", true) => is_alive = false
      }
    }
  }
  tracer.addTrace(windowCenter, trace)

  private def move(delta:Vec) {
    tracer.moveTrace(trace, delta)
  }
  key(KEY_W, 10, onKeyDown = {if(!onPause) move(Vec(0,1))})
  key(KEY_A, 10, onKeyDown = {if(!onPause) move(Vec(-1,0))})
  key(KEY_S, 10, onKeyDown = {if(!onPause) move(Vec(0,-1))})
  key(KEY_D, 10, onKeyDown = {if(!onPause) move(Vec(1,0))})

  private def shootPoint = (mouseCoord - trace.location).n*(player_radius+5) + trace.location
  leftMouse(30, onBtnDown = {
    mouse_coord => new Bullet(shootPoint, mouse_coord - shootPoint, shooter_type = "player")
  })

  private var is_alive = true
  def isAlive = is_alive

  init {
    is_alive = true
    tracer.updateLocation(trace, windowCenter)
    Unit
  }

  render {
    currentColor = BLACK
    drawLine(trace.location, shootPoint)
    drawCircle(trace.location, player_radius)
  }
}