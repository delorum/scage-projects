package com.github.dunnololda.scageprojects.orbitalkiller

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ShipsHolder {
  def addShip(ship:PolygonShip): Unit = {
    _ships += ship
    _shipsMap += (ship.index -> ship)
  }
  def removeShip(ship:PolygonShip): Unit = {
    _ships -= ship
    _shipsMap -= ship.index
  }
  def removeShipByIndex(ship_index:Int): Unit = {
    _shipsMap.remove(ship_index).foreach(ship => _ships -= ship)
  }

  def ships:Seq[PolygonShip] = _ships

  private val _ships = ArrayBuffer[PolygonShip]()
  private val _shipsMap = mutable.HashMap[Int, PolygonShip]()//ships.map(s => (s.index, s)).toMap
  def shipByIndex(index:Int):Option[PolygonShip] = _shipsMap.get(index)
}
