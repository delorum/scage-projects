package com.github.dunnololda.scageprojects.orbitalkiller.components

import BasicComponents._

/**
  * Created by andrey on 1/7/18.
  */
class TimeMultiplier(shipComponents: ShipComponents) {
  private var _time_multiplier = realtime

  def timeMultiplier = {
    /*if (_time_multiplier != realtime && ShipsHolder.ships.flatMap(_.engines).exists(_.active)) {
      timeMultiplier_=(realtime)
    }*/
    _time_multiplier
  }

  def timeMultiplier_=(new_time_multiplier: Int) {
    if (new_time_multiplier > 0) {
      _time_multiplier = new_time_multiplier
      shipComponents.ships.flatMap(_.engines).filter(_.active).foreach(e => {
        e.workTimeTacts = e.workTimeTacts
      })
      /*}*/
    }
  }

  /*def maxTimeMultiplier:Int = {
    /*val a = ships.map(s => math.abs(s.currentState.acc*s.currentState.vel.p)).max
    math.max((0.1*math.pow(351.3011068768212/a, 10.0/7)/5).toInt, 1)*/
    1
  }*/

  def foreach(func: Int => Any): Unit = {
    (1 to _time_multiplier).foreach(func)
  }

  def isRealTime: Boolean = _time_multiplier == realtime

  def setToRealTime(mul: Int = 1): Unit = {
    _time_multiplier = realtime * mul
  }

  def incByRealTime(mul: Int = 1): Unit = {
    _time_multiplier += realtime * mul
  }

  def decByRealTime(): Unit = {
    _time_multiplier = math.max(_time_multiplier - realtime, realtime)
  }
}
