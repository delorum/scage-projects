package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import OrbitalKiller._

class Ship2(
  index:String,
  init_coord:Vec,
  init_velocity:Vec = Vec.zero,
  init_rotation:Float = 0f
) extends PolygonShip(index, init_coord, init_velocity, init_rotation) {
  val points:List[Vec] = List(
    Vec(-10.0, 70.74075),
    Vec(10.0, 70.74075),
    Vec(10.0, 10.740753),
    Vec(50.0, -9.259247),
    Vec(10.0, -9.259247),
    Vec(10.0, -69.25925),
    Vec(-10.0, -69.25925),
    Vec(-10.0, -9.259247),
    Vec(-50.0, -9.259247),
    Vec(-10.0, 10.740753),
    Vec(-10.0, 70.74075)
  )

  val eight = Engine(Vec(0.0, 70.74075), force_dir = Vec(0.0, -1.0), max_power = 10, this)
  val two = Engine(position = Vec(0.0, -69.25925), force_dir = Vec(0.0, 1.0), max_power = 10, this)

  val four = Engine(position = Vec(-40.0, -9.259247), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val six =  Engine(position = Vec(40.0, -9.259247), force_dir = Vec(0.0, 1.0), max_power = 10, this)

  val engines = List(two, four, six, eight)

  val engines_mapping = Map(
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD8 -> eight,

    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six
  )

  def rotateRight() {
    activateOnlyOneEngine(four)
  }

  def smallRotateRight() {
    activateOnlyOneEngine(four)
  }

  def rotateLeft() {
    activateOnlyOneEngine(six)
  }

  def smallRotateLeft() {
    activateOnlyOneEngine(six)
  }
}
