package com.github.dunnololda.scageprojects.orbitalkiller

import OrbitalKiller._
import com.github.dunnololda.scage.ScageLib._

case class Engine(position:DVec, force_dir:DVec, max_power:Double, ship:Ship) {
  private var worktime_tacts = 0l
  private var stop_moment_tacts = 0l

  def worktimeTacts = worktime_tacts
  def worktimeTacts_=(new_worktime_tacts:Long) {
    worktime_tacts = new_worktime_tacts
    stop_moment_tacts = tacts + worktime_tacts*timeMultiplier
  }

  def stopMomentSeconds = stop_moment_tacts

  private var _power:Double = 0.0
  def power = _power
  def power_=(new_power:Double) {
    if(new_power >= 0 && new_power <= max_power && new_power != _power) {
      _power = new_power
    }
  }

  def powerPercent:Long = math.round(_power/max_power*100)
  def powerPercent_=(new_power_percent:Long) {
    if(new_power_percent >= 0 && new_power_percent <= 100) {
      val new_power = max_power*new_power_percent/100f
      //println(s"$new_power_percent : $new_power")
      _power = new_power
    }
  }

  def force = force_dir*power
  def torque = -force*/position

  private var is_active:Boolean = false
  def active = is_active
  def active_=(bool:Boolean) {
    if(is_active != bool) {
      is_active = bool
      if(is_active) {
        //_power = 1f
        worktimeTacts = 10
        ship.selected_engine = Some(this)
      } else {
        ship.selected_engine = ship.engines.filter(_.active).lastOption
      }
      updateFutureTrajectory()
    }
  }
  def switchActive() {
    if(!is_active) {
      is_active = true
      //_power = 1f
      worktimeTacts = 10
      ship.selected_engine = Some(this)
      updateFutureTrajectory()
    } else {
      if (!ship.isSelectedEngine(this)) ship.selected_engine = Some(this)
      else {
        is_active = false
        ship.selected_engine = ship.engines.filter(_.active).lastOption
        updateFutureTrajectory()
      }
    }
  }

  action {
    if(is_active) {
      if(worktime_tacts > 0) worktime_tacts -= 1
      else active = false
    }
  }
}