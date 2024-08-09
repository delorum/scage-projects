package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder

import com.github.dunnololda.scageprojects.orbitalkiller.PolygonShip
import com.github.dunnololda.scageprojects.orbitalkiller_cake.ObjectIndices.{earthIndex, moonIndex, sunIndex}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.{Planet, PlanetWithAir, Star}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.MutableBodyState
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.SystemEvolution

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ShipsHolder(system_evolution: SystemEvolution, sun: Star, earth: PlanetWithAir, moon: Planet) {

  def addShip(ship: PolygonShip): Unit = {
    _ships += ship
    _shipsMap += (ship.index -> ship)
    _shipIndicies += ship.index
    system_evolution.addBody(
      ship.currentState,
      (tacts, helper) => {
        helper.gravityForceFromTo(sunIndex, ship.index) +
          helper.gravityForceFromTo(earthIndex, ship.index) +
          helper.gravityForceFromTo(moonIndex, ship.index) +
          helper.funcOrDVecZero(ship.index, bs => ship.currentReactiveForce(tacts, bs)) +
          helper.funcOfArrayOrDVecZero(
            Array(ship.index, earthIndex),
            l => {
              val bs = l(0)
              val e = l(1)
              // val other_ship_states = helper.bodyStates(ShipsHolder.shipIndicies.filterNot(_ == ship.index))
              earth.airResistance(bs, e, /*other_ship_states, */ 28.0, 0.5)
            }
          )
      },
      (tacts, helper) => {
        helper.funcOrDoubleZero(ship.index, _ => ship.currentTorque(tacts))
      },
      () => ship.checkCriticalCollision()
    )
  }

  def removeShip(ship: PolygonShip): Unit = {
    removeShipByIndex(ship.index)
  }

  def removeShipByIndex(ship_index: Int): Unit = {
    _shipsMap
      .remove(ship_index)
      .foreach(ship => {
        println(s"removed ship ${ship.name}")
        update_list = true
        system_evolution.removeBodyByIndex(ship_index)
      })
    _shipIndicies -= ship_index
  }

  private var update_list: Boolean = false

  def ships: Seq[PolygonShip] = {
    if (update_list) {
      _ships --= _ships.filter(s => !_shipsMap.contains(s.index))
      update_list = false
    }
    _ships
  }

  private val _ships = ArrayBuffer[PolygonShip]()
  private val _shipsMap = mutable.HashMap[Int, PolygonShip]()

  // ships.map(s => (s.index, s)).toMap
  def shipByIndex(index: Int): Option[PolygonShip] = _shipsMap.get(index)

  private val _shipIndicies: mutable.HashSet[Int] = mutable.HashSet[Int]()

  def shipIndicies: mutable.Set[Int] = _shipIndicies

  def currentShipStatesExceptShip(ship_index: Int): Seq[MutableBodyState] =
    _ships.withFilter(x => x.currentState.active && x.index != ship_index).map(_.currentState)
}
