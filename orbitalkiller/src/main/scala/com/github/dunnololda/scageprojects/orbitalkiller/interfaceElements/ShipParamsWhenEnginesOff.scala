package com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

class ShipParamsWhenEnginesOff extends InterfaceElement {
  def linearSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != 1 || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        getFutureState(ship.engines.map(_.stopMomentTacts).max).get(ship.index) match {
          case Some(bs) =>
            val s = bs.vel
            //f"${msecOrKmsec(s.norma)} (velx = ${msecOrKmsec(s.x)}, vely = ${msecOrKmsec(s.y)})"
            msecOrKmsec(s.norma)
          case None => "N/A"
        }
      } else {
        //f"${msecOrKmsec(ship.linearVelocity.norma)} (velx = ${msecOrKmsec(ship.linearVelocity.x)}, vely = ${msecOrKmsec(ship.linearVelocity.y)})"
        msecOrKmsec(ship.linearVelocity.norma)
      }
    }
  }

  def angularSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != 1 || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        getFutureState(ship.engines.map(_.stopMomentTacts).max).get(ship.index) match {
            case Some(bs) =>
              val s = bs.ang_vel
              f" $s%.2f град/сек"
            case None => "N/A"
          }
        } else {
        f" ${ship.angularVelocity}%.2f град/сек"
      }
    }
  }

  def orbitParametersStrWhenEnginesOff:String = {
    if(ship.flightMode != 1 || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        val lbs = getFutureState(ship.engines.map(_.stopMomentTacts).max)
        lbs.get(ship.index) match {
            case Some(bs) =>
              orbitStrInPointWithVelocity(bs.coord, bs.vel, bs.mass, lbs.filter(x => planet_indexes.contains(x._1)))
            case None => "N/A"
          }
        } else {
        orbitStrInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass, currentPlanetStates)
      }
    }
  }

  def fuelMassWhenEnginesOff:String = {
    if(ship.flightMode != 1 || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        f"${ship.fuelMass - ship.engines.filter(e => e.active).map(e => e.workTimeTacts*e.fuelConsumptionPerTact).sum}%.1f кг"
      } else {
        f"${ship.fuelMass}%.1f кг"
      }
    }
  }

  private val strings = Array("", "", "", "", "")
  override protected def _update(): Unit = {
    if (ship.flightMode != 0) {
      val engines_active = ship.engines.exists(_.active)
      if(engines_active) {
        strings(0) = f"Линейная скорость в момент отключения двигателей: $linearSpeedStrWhenEnginesOff"
        strings(1) = f"Угловая скорость в момент отключения двигателей: $angularSpeedStrWhenEnginesOff"
        strings(2) = s"Параметры орбиты в момент отключения двигателей:"
        strings(3) = orbitParametersStrWhenEnginesOff
        strings(4) = s"Остаток топлива в момент отключения двигателей: $fuelMassWhenEnginesOff"
      }
    }
  }

  override def data: Seq[String] = strings

  override val shortDescr: String = "SP"
}
