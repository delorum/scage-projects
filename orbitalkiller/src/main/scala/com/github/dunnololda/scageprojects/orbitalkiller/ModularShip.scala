package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

sealed trait Module {
  def position:Vec
}

case class Engine2(position:Vec, force_dir:Vec, max_power:Float) {
  private var worktime_tacts = 0l
  private var stop_moment_seconds = 0l

  def worktimeTacts = worktime_tacts
  def worktimeTacts_=(new_worktime_tacts:Long) {
    worktime_tacts = new_worktime_tacts
    stop_moment_seconds = tacts + worktime_tacts*timeMultiplier
  }

  def stopMomentSeconds = stop_moment_seconds

  private var _power:Float = 1f
  def power = _power
  def power_=(new_power:Float) {
    if(new_power >= 1f && new_power < max_power && new_power != _power) {
      _power = new_power
      updateFutureTrajectory()
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
        _power = 1f
        if(worktime_tacts == 0) {
          worktimeTacts = 10
        }
      }
      updateFutureTrajectory()
    }
  }
  def switchActive() {
    if(!is_active) {
      is_active = true
      _power = 1f
      worktimeTacts = 10
      updateFutureTrajectory()
    } else {
      is_active = false
      updateFutureTrajectory()
    }
  }

  action {
    if(is_active) {
      if(worktime_tacts > 0) worktime_tacts -= 1
      else active = false
    }
  }
}

case class EngineModule(position:Vec, engine_number:Int, engine_side:Int, engine_max_power:Float) extends Module {
  val engine = Engine2(
    position = engine_side match {
      case 0 => Vec(-5, 0)
      case 1 => Vec(0, 5)
      case 2 => Vec(5, 0)
      case 3 => Vec(0, -5)
      case _ => Vec(-5, 0)
    },
    force_dir = engine_side match {
      case 0 => Vec(1, 0)
      case 1 => Vec(0, -1)
      case 2 => Vec(-1, 0)
      case 3 => Vec(0, 1)
      case _ => Vec(1, 0)
    },
    max_power = engine_max_power
  )
}

case class EmptyModule(position:Vec) extends Module

class ModularShip(val index:String, init_coord:Vec, init_velocity:Vec = Vec.zero, init_rotation:Float = 0f, modules:List[Module], vertices:List[Vec]) {

}
