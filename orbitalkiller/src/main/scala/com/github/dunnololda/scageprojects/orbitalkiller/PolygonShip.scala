package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

abstract class PolygonShip(
  val index:String,
  init_coord:Vec,
  init_velocity:Vec = Vec.zero,
  init_rotation:Float = 0f) extends Ship {
  def points:List[Vec]
  def engines:List[Engine]
  def engines_mapping:Map[Int, Engine]

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = init_rotation,
      shape = PolygonShape(points),
      is_static = false)
  )
}
