package com.github.dunnololda.scageprojects.orbitalkiller_cake

import com.github.dunnololda.scage.support.ScageId

object ObjectIndices {
  val sunIndex: Int = ScageId.nextId
  val earthIndex: Int = ScageId.nextId
  val moonIndex: Int = ScageId.nextId
  val playerShipIndex: Int = ScageId.nextId
  val stationIndex: Int = ScageId.nextId
  val sat1Index: Int = ScageId.nextId
  val sat2Index: Int = ScageId.nextId
  val cargo1Index: Int = ScageId.nextId

  val planetIndices: Set[Int] = Set(sunIndex, earthIndex, moonIndex)

  val shipIndices: Set[Int] = Set(sunIndex, earthIndex, moonIndex)

  val allIndices: Set[Int] =
    Set(sunIndex, earthIndex, moonIndex, playerShipIndex, stationIndex, sat1Index, sat2Index, cargo1Index)
}
