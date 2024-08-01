package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.ScageId
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.{Constants, Main}

class Engine(
    val name: Int,
    val position: DVec, // позиция относительно центра массы корабля (ц.м. в точке (0,0))
    val force_dir: DVec, // вектор направления приложения силы
    val max_power: Double, // в ньютонах
    val default_power_percent: Int, // при выборе данного двигателя какая мощность выставляется по умолчанию
    val fuel_consumption_per_sec_at_full_power: Double, // Расход топлива в килограммах в секунду на полной мощности
    val ship: PolygonShip) {
  val index: Int = ScageId.nextId
  private var worktime_tacts = 0L
  private var stop_moment_tacts = 0L

  def workTimeMsec = (worktime_tacts * Constants.base_dt * 1000).toLong

  def workTimeStr = timeStrMsec((worktime_tacts * Constants.base_dt * 1000).toLong)

  def workTimeTacts = worktime_tacts

  def workTimeTacts_=(new_worktime_tacts: Long) {
    if (new_worktime_tacts >= 0) {
      val prev = worktime_tacts
      worktime_tacts = new_worktime_tacts
      stop_moment_tacts = system_evolution.tacts + worktime_tacts
      if (ship.fuelMassWhenEnginesOff < 0 && new_worktime_tacts > prev) {
        val possible_fuel_for_this_engine = ship.fuelMassWhenEnginesOffWithoutEngine(this)
        if (worktime_tacts > prev && possible_fuel_for_this_engine > 0) {
          worktime_tacts = (possible_fuel_for_this_engine / fuelConsumptionPerTact).toLong
          stop_moment_tacts = system_evolution.tacts + worktime_tacts
        } else {
          worktime_tacts = prev
          stop_moment_tacts = system_evolution.tacts + worktime_tacts
        }
      }
    }
  }

  def maxFuelConsumptionPerTact: Double = {
    fuel_consumption_per_sec_at_full_power * Constants.base_dt
  }

  def fuelConsumptionPerTact: Double = {
    fuel_consumption_per_sec_at_full_power * (_power / max_power) * Constants.base_dt
  }

  def fuelConsumptionPerSec: Double = {
    fuel_consumption_per_sec_at_full_power * (_power / max_power)
  }

  def stopMomentTacts = stop_moment_tacts

  private var _power: Double = 0.0

  def power = _power

  def power_=(new_power: Double) {
    if (new_power >= 0 && new_power <= max_power && new_power != _power) {
      val prev = _power
      _power = new_power
      if (ship.fuelMassWhenEnginesOff < 0) {
        _power = prev
      }
    }
  }

  def powerPercent: Long = math.round(_power / max_power * 100)

  def powerPercent_=(new_power_percent: Long) {
    if (new_power_percent >= 0 && new_power_percent <= 100) {
      val new_power = {
        if (InterfaceHolder.gSwitcher.maxGSet) {
          math.min(
            max_power * new_power_percent / 100.0,
            ship.thisOrActualProxyShipMass * InterfaceHolder.gSwitcher.maxG * Main.earth.g + {
              earth
                .airResistance(
                  ship.currentState,
                  earth.currentState, /*ShipsHolder.currentShipStatesExceptShip(ship.index), */ 28,
                  0.5
                )
                .norma
            }
          )
        } else {
          max_power * new_power_percent / 100.0
        }
      }
      val prev = _power
      _power = new_power
      if (ship.fuelMassWhenEnginesOff < 0) {
        _power = prev
      } else {
        if (
          (ship.flightMode == FreeFlightMode || ship.flightMode == Maneuvering) && InterfaceHolder.gSwitcher.maxGSet
        ) {
          ship.syncOtherEnginesPower(index)
        }
      }
    }
  }

  def force = force_dir * power

  def torque = -force */ position

  private var is_active: Boolean = false

  def active = is_active

  def active_=(bool: Boolean) {
    if (is_active != bool) {
      if (bool) {
        if (!ship.engineDisabled(index) && ship.fuelMass > fuelConsumptionPerTact) {
          is_active = true
          if (power == 0) {
            powerPercent = default_power_percent
          }
          // timeMultiplier = realtime
          if (workTimeTacts == 0) workTimeTacts = 10
          if (
            (ship.flightMode == FreeFlightMode || ship.flightMode == Maneuvering) && InterfaceHolder.gSwitcher.maxGSet
          ) {
            ship.syncOtherEnginesPower(index)
          }
        } else {
          is_active = false
        }
      } else {
        is_active = false
      }
      needToUpdateOrbits("engine active")
    }
  }

  def switchActive() {
    active = !active
  }
}
