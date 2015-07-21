package com.github.dunnololda.scageprojects.orbitalkiller

import OrbitalKiller._
import com.github.dunnololda.scage.ScageLibD._

case class Engine(position:DVec, force_dir:DVec, max_power:Double, default_power_percent:Int, ship:Ship) {
  private var worktime_tacts = 0l
  private var stop_moment_tacts = 0l

  def workTimeMsec  = (worktime_tacts*base_dt*1000).toLong
  def workTimeStr   = timeStr((worktime_tacts*base_dt*1000).toLong)
  def workTimeTacts = worktime_tacts
  def workTimeTacts_=(new_worktime_tacts:Long) {
    if(new_worktime_tacts >= 0) {
      val prev = worktime_tacts
      worktime_tacts = new_worktime_tacts
      stop_moment_tacts = tacts + worktime_tacts * timeMultiplier
      if(ship.fuelMassWhenEnginesOff < 0) {
        val possible_fuel_for_this_engine = ship.fuelMassWhenEnginesOffWithoutEngine(this)
        if(worktime_tacts > prev && possible_fuel_for_this_engine > 0) {
          worktime_tacts = (possible_fuel_for_this_engine / fuelConsumptionPerTact).toLong
          stop_moment_tacts = tacts + worktime_tacts * timeMultiplier
        } else {
          worktime_tacts = prev
          stop_moment_tacts = tacts + worktime_tacts * timeMultiplier
        }
      }
    }
  }

  /**
   * Расход топлива в килограммах в секунду на полной мощности
   *
   * */
  val fuel_consumption_per_sec_at_full_power:Double = 200
  
  def fuelConsumptionPerTact:Double = {
    fuel_consumption_per_sec_at_full_power*(_power/max_power)*base_dt*timeMultiplier
  }

  def stopMomentTacts = stop_moment_tacts

  private var _power:Double = 0.0
  def power = _power
  def power_=(new_power:Double) {
    if(new_power >= 0 && new_power <= max_power && new_power != _power) {
      val prev = _power
      _power = new_power
      if(ship.fuelMassWhenEnginesOff < 0) {
        _power = prev
      }
    }
  }

  def powerPercent:Long = math.round(_power/max_power*100)
  def powerPercent_=(new_power_percent:Long) {
    if(new_power_percent >= 0 && new_power_percent <= 100) {
      val new_power = max_power*new_power_percent/100.0
      val prev = _power
      _power = new_power
      if(ship.fuelMassWhenEnginesOff < 0) {
        _power = prev
      }
    }
  }

  def force = force_dir*power
  def torque = -force*/position

  private var is_active:Boolean = false
  def active = is_active
  def active_=(bool:Boolean) {
    if(is_active != bool) {
      if(bool) {
        if(ship.fuelMass > fuelConsumptionPerTact) {
          is_active = true
          if(power == 0) {
            powerPercent = default_power_percent
          }
          timeMultiplier = realtime
          if(workTimeTacts == 0) workTimeTacts = 10
          ship.selected_engine = Some(this)
        } else {
          is_active = false
          ship.selected_engine = ship.engines.filter(_.active).lastOption
        }
      } else {
        is_active = false
        ship.selected_engine = ship.engines.filter(_.active).lastOption
      }
      //updateFutureTrajectory("engine active")
    }
  }
  def switchActive() {
    active = !active
  }

  action(position = -1) {
    if(is_active) {
      if(worktime_tacts <= 0 || ship.fuelMass <= 0) {
        active = false
      } else {
        if(ship.fuelMass - fuelConsumptionPerTact <= 0) {
          active = false
        }
      }
    }
  }

  action(position = 1) {
    if(is_active) {
      if(worktime_tacts > 0) {
        worktime_tacts -= 1
        ship.fuelMass -= fuelConsumptionPerTact
      } else active = false
    }
  }
}
