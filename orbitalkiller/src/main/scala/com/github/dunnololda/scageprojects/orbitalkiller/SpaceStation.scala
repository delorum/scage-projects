package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

class SpaceStation(
             index:String,
             init_coord:Vec,
             init_velocity:Vec = Vec.zero,
             init_rotation:Float = 0f
             ) extends PolygonShip(index, init_coord, init_velocity, init_rotation) {
  val points:List[Vec] = List(
    Vec(-50.0, 50.0),
    Vec(-30.0, 50.0),
    Vec(-30.0, 30.0),
    Vec(-10.0, 10.0),
    Vec(-10.0, -10.0),
    Vec(-30.0, -30.0),
    Vec(-30.0, -50.0),
    Vec(-50.0, -50.0),
    Vec(-50.0, -110.0),
    Vec(-30.0, -110.0),
    Vec(-30.0, -70.0),
    Vec(30.0, -70.0),
    Vec(30.0, -110.0),
    Vec(50.0, -110.0),
    Vec(50.0, -50.0),
    Vec(30.0, -50.0),
    Vec(30.0, -30.0),
    Vec(10.0, -10.0),
    Vec(10.0, 10.0),
    Vec(30.0, 30.0),
    Vec(30.0, 50.0),
    Vec(50.0, 50.0),
    Vec(50.0, 110.0),
    Vec(30.0, 110.0),
    Vec(30.0, 90.0),
    Vec(30.0, 70.0),
    Vec(10.0, 70.0),
    Vec(-10.0, 70.0),
    Vec(-30.0, 70.0),
    Vec(-30.0, 90.0),
    Vec(-30.0, 110.0),
    Vec(-50.0, 110.0)
  )

  val four = Engine(position = Vec(-10.0, 0.0), force_dir = Vec(1.0, 0.0), max_power = 10, this)
  val six = Engine(position = Vec(10.0, 0.0), force_dir = Vec(-1.0, 0.0), max_power = 10, this)
  val seven = Engine(position = Vec(-40.0, 110.0), force_dir = Vec(0.0, -1.0), max_power = 10, this)
  val nine = Engine(position = Vec(40.0, 110.0), force_dir = Vec(0.0, -1.0), max_power = 10, this)
  val one = Engine(position = Vec(-40.0, -110.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val three = Engine(position = Vec(40.0, -110.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)

  val engines = List(four, six, seven, nine, one, three)

  val engines_mapping = Map(
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

  def rotateRight() {
    activateOnlyTheseEngines(one, seven)
  }

  def smallRotateRight() {
    activateOnlyOneEngine(seven)
  }

  def rotateLeft() {
    activateOnlyTheseEngines(three, nine)
  }

  def smallRotateLeft() {
    activateOnlyOneEngine(nine)
  }
}
