package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import OrbitalKiller._

class Ship3(
             index:String,
             init_coord:Vec,
             init_velocity:Vec = Vec.zero,
             init_rotation:Float = 0f
             ) extends PolygonShip(index, init_coord, init_velocity, init_rotation) {
  val points:List[Vec] = List(
    Vec(-10.0, 70.0),
    Vec(10.0, 70.0),
    Vec(10.0, 30.0),
    Vec(50.0, 10.0),
    Vec(10.0, 10.0),
    Vec(10.0, -10.0),
    Vec(50.0, -30.0),
    Vec(10.0, -30.0),
    Vec(10.0, -70.0),
    Vec(-10.0, -70.0),
    Vec(-10.0, -30.0),
    Vec(-50.0, -30.0),
    Vec(-10.0, -10.0),
    Vec(-10.0, 10.0),
    Vec(-50.0, 10.0),
    Vec(-10.0, 30.0)
  )

  val eight = Engine(position = Vec(0.0, 70.0), force_dir = Vec(0.0, -1.0), max_power = 10, this)
  val two = Engine(position = Vec(0.0, -70.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val four = Engine(position = Vec(-10.0, 0.0), force_dir = Vec(1.0, 0.0), max_power = 10, this)
  val six = Engine(position = Vec(10.0, 0.0), force_dir = Vec(-1.0, 0.0), max_power = 10, this)
  val seven = Engine(position = Vec(-40.0, 10.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val nine = Engine(position = Vec(40.0, 10.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val one = Engine(position = Vec(-40.0, -30.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val three = Engine(position = Vec(40.0, -30.0), force_dir = Vec(0.0, 1.0), max_power = 10, this)

  val engines = List(eight, two, four, six, seven, nine, one, three)

  val engines_mapping = Map(
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

  def rotateRight() {
    activateOnlyTheseEngines(one, eight)
  }

  def smallRotateRight() {
    activateOnlyTheseEngines(seven, eight)
  }

  def rotateLeft() {
    activateOnlyTheseEngines(three, eight)
  }

  def smallRotateLeft() {
    activateOnlyTheseEngines(nine, eight)
  }

  private def howManyTacts(to:Float, from:Float, a:Float, dt:Float, tacts:Int = 0):Int = {
    if(a == 0) tacts
    else if(a > 0) {
      if(from >= to) tacts
      else howManyTacts(to, from + a*dt, a, dt, tacts+1)
    } else {
      if(from <= to) tacts
      else howManyTacts(to, from + a*dt, a, dt, tacts+1)
    }
  }

  override def preserveAngularVelocity(ang_vel_deg:Float) {
    val difference = angularVelocity*60f*base_dt - ang_vel_deg
    if(difference > 0.01f) {
      val ang_acc = seven.torque
      val tacts = howManyTacts(ang_vel_deg/60f/base_dt, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(seven, eight)
      seven.worktimeTacts = tacts
      eight.worktimeTacts = tacts
    } else if(difference < -0.01f) {
      val ang_acc = nine.torque
      val tacts = howManyTacts(ang_vel_deg/60f/base_dt, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(nine, eight)
      nine.worktimeTacts = tacts
      eight.worktimeTacts = tacts
    }
  }
}
