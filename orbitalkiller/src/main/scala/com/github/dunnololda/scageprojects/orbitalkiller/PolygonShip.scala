package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

abstract class PolygonShip(
  val index:String,
  init_coord:DVec,
  init_velocity:DVec = DVec.dzero,
  init_rotation:Double = 0) extends Ship {
  def points:List[DVec]
  def engines:List[Engine]
  def engines_mapping:Map[Int, Engine]

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0,
      ang_vel = 1,
      ang = init_rotation,
      shape = PolygonShape(points),
      is_static = false)
  )
}
