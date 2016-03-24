package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceElement
import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

class ShipParamsWhenEnginesOff extends InterfaceElement {
  def linearSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != FreeFlightMode || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        val future_state = getFutureState(ship.engines.map(_.stopMomentTacts).max)
        val future_planet_states = planetStates(future_state)
        future_state.get(ship.index) match {
          case Some(bs) =>
            insideSphereOfInfluenceOfCelestialBody(bs.coord, bs.mass, future_planet_states) match {
              case Some((planet, planet_state)) =>
                s"${msecOrKmsec((bs.vel - planet_state.vel).norma)} (${planet.name}), ${msecOrKmsec(bs.vel.norma)} (абсолютная)"
              case None =>
                s"${msecOrKmsec(bs.vel.norma)} (абсолютная)"
            }
          case None => "N/A"
        }
      } else {
        //f"${msecOrKmsec(ship.linearVelocity.norma)} (velx = ${msecOrKmsec(ship.linearVelocity.x)}, vely = ${msecOrKmsec(ship.linearVelocity.y)})"
        msecOrKmsec(ship.linearVelocity.norma)
      }
    }
  }

  def angularSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != FreeFlightMode || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
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
    if(ship.flightMode != FreeFlightMode || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        val lbs = getFutureState(ship.engines.map(_.stopMomentTacts).max)
        lbs.get(ship.index) match {
            case Some(bs) =>
              val celestials = lbs.filter(kv => planet_indexes.contains(kv._1)).flatMap(kv => {
                planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
              }).values.toSeq.sortBy(_._2.mass)
              orbitStrInPointWithVelocity(bs.coord, bs.vel, bs.mass, celestials)
            case None => "N/A"
          }
        } else {
        orbitStrInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass, currentPlanetStates)
      }
    }
  }

  def fuelMassWhenEnginesOff:String = {
    if(ship.flightMode != FreeFlightMode || anyEngineKeyPressed) "N/A"  // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
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
    if (ship.flightMode != Maneuvering) {
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
