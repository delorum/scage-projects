package com.github.dunnololda.scageprojects.orbitalkiller.interface.elements

import com.github.dunnololda.scageprojects.orbitalkiller._
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ObjectIndices.planetIndices
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsHelper
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ships.FlightMode.{FreeFlightMode, Maneuvering}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils._
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics.GravityUtils.insideSphereOfInfluenceOfCelestialBody
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics.OrbitUtils.orbitStrInPointWithVelocity

class ShipParamsWhenEnginesOff(celestialsHelper: CelestialsHelper, playerShip: Ship4) extends InterfaceElement {

  private def linearSpeedStrWhenEnginesOff: String = {
    if (playerShip.flightMode != FreeFlightMode || anyEngineKeyPressed)
      "N/A" // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if (playerShip.engines.exists(_.active)) {
        val future_state = getFutureState(playerShip.engines.map(_.stopMomentTacts).max)
        val future_planet_states = celestialsHelper.planetStates(future_state)
        future_state.get(playerShip.thisOrActualProxyShipIndex) match {
          case Some(bs) =>
            insideSphereOfInfluenceOfCelestialBody(bs.coord, bs.mass, future_planet_states) match {
              case Some((planet, planet_state)) =>
                s"${msecOrKmsecOrKmhour((bs.vel - planet_state.vel).norma)} (${planet.name}), ${msecOrKmsecOrKmhour(bs.vel.norma)} (абсолютная)"
              case None =>
                s"${msecOrKmsecOrKmhour(bs.vel.norma)} (абсолютная)"
            }
          case None => "N/A"
        }
      } else {
        // f"${msecOrKmsec(ship.linearVelocity.norma)} (velx = ${msecOrKmsec(ship.linearVelocity.x)}, vely = ${msecOrKmsec(ship.linearVelocity.y)})"
        msecOrKmsecOrKmhour(playerShip.linearVelocity.norma)
      }
    }
  }

  private def angularSpeedStrWhenEnginesOff: String = {
    if (playerShip.flightMode != FreeFlightMode || anyEngineKeyPressed)
      "N/A" // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if (playerShip.engines.exists(_.active)) {
        getFutureState(playerShip.engines.map(_.stopMomentTacts).max)
          .get(playerShip.thisOrActualProxyShipIndex) match {
          case Some(bs) =>
            val s = bs.ang_vel
            f" $s%.2f град/сек"
          case None => "N/A"
        }
      } else {
        f" ${playerShip.angularVelocity}%.2f град/сек"
      }
    }
  }

  private def orbitParametersStrWhenEnginesOff: String = {
    if (playerShip.flightMode != FreeFlightMode || anyEngineKeyPressed)
      "N/A" // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if (playerShip.engines.exists(_.active)) {
        val lbs = getFutureState(playerShip.engines.map(_.stopMomentTacts).max)
        lbs.get(playerShip.thisOrActualProxyShipIndex) match {
          case Some(bs) =>
            val celestials = lbs
              .filter(kv => planetIndices.contains(kv._1))
              .flatMap(kv => {
                celestialsHelper.planets.get(kv._1).map(planet => (kv._1, (planet, kv._2)))
              })
              .values
              .toSeq
              .sortBy(_._2.mass)
            orbitStrInPointWithVelocity(bs.coord, bs.vel, playerShip.radius, bs.mass, celestials)
          case None => "N/A"
        }
      } else {
        playerShip.thisOrActualProxyShipOrbitData.map(_.orbitStrDefinition).getOrElse("N/A")
      }
    }
  }

  private def fuelMassWhenEnginesOff: String = {
    if (playerShip.flightMode != FreeFlightMode || anyEngineKeyPressed)
      "N/A" // только в свободном режиме и если не нажаты клавиши управления двигателями отображать инфу
    else {
      if (playerShip.engines.exists(_.active)) {
        f"${playerShip.fuelMass - playerShip.engines.filter(e => e.active).map(e => e.workTimeTacts * e.fuelConsumptionPerTact).sum}%.1f кг"
      } else {
        f"${playerShip.fuelMass}%.1f кг"
      }
    }
  }

  private val strings = Array("", "", "", "", "")

  override protected def _update(): Unit = {
    if (playerShip.flightMode != Maneuvering) {
      val engines_active = playerShip.engines.exists(_.active)
      if (engines_active) {
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
