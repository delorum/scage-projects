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

  private def howManyTacts(to:Float, from:Float, a:Float, dt:Float):(Int, Float) = {
    val tacts = ((to - from)/(a*dt)).toInt + 1
    val result_to = from + tacts*a*dt
    (tacts, result_to)
    /*if(a == 0) tacts
    else if(a > 0) {
      if(from >= to) tacts
      else howManyTacts(to, from + a*dt, a, dt, tacts+1)
    } else {
      if(from <= to) tacts
      else howManyTacts(to, from + a*dt, a, dt, tacts+1)
    }*/
  }

  private def maxPossiblePower(force_dir:Float, mass:Float, to:Float, from:Float, max_diff:Float):Float = {
    val powers = (9.9f to 1f by -0.1f).filter {
      case pp =>
        val force = force_dir*pp
        val acc = force / mass
        val (_, result_to) = howManyTacts(to, from, acc, dt)
        math.abs(to - result_to) < max_diff
    }
    powers.max
  }

  override def preserveAngularVelocity(ang_vel_deg:Float) {
    val difference = angularVelocity*60f*base_dt - ang_vel_deg
    if(difference > 0.01f) {
      seven.power = 1f
      eight.power = 1f
      val ang_acc = (seven.torque / currentState.I)/math.Pi.toFloat*180f
      val (tacts, _) = howManyTacts(ang_vel_deg/60f/base_dt, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(seven, eight)
      seven.worktimeTacts = tacts
      eight.worktimeTacts = tacts
    } else if(difference < -0.01f) {
      nine.power = 1f
      eight.power = 1f
      val ang_acc = (nine.torque / currentState.I)/math.Pi.toFloat*180f
      val (tacts, _) = howManyTacts(ang_vel_deg/60f/base_dt, angularVelocity, ang_acc, dt)
      activateOnlyTheseEngines(nine, eight)
      nine.worktimeTacts = tacts
      eight.worktimeTacts = tacts
    }
  }

  def enterOrbit() {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(body) =>
        val ss = satelliteSpeed(coord, body.coord, body.linearVelocity, body.mass, G)
        val n = Vec(0, 1).rotateDeg(rotation).n
        val p = n.p*(-1)

        val ship_velocity_n = linearVelocity*n  // from
        val ss_n = ss*n                         // to

        if(ship_velocity_n > ss_n) {
          val power = maxPossiblePower(eight.force_dir.y, mass, ss_n, ship_velocity_n, 0.1f)

          eight.power = power
          val acc = (eight.force / mass).y
          val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, dt)
          println("===========================")
          println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")
          eight.active = true
          eight.worktimeTacts = tacts
        } else if(ship_velocity_n < ss_n) {
          val power = maxPossiblePower(two.force_dir.y, mass, ss_n, ship_velocity_n, 0.1f)
          two.power = power
          val acc = (two.force / mass).y
          val (tacts, result_to) = howManyTacts(ss_n, ship_velocity_n, acc, dt)
          println("===========================")
          println(s"$ship_velocity_n -> $ss_n : $tacts : $result_to : $power")
          two.active = true
          two.worktimeTacts = tacts
        }

        val ship_velocity_p = p*linearVelocity
        val ss_p = p*ss

        if(ship_velocity_p > ss_p) {
          val power = maxPossiblePower(six.force_dir.x, mass, ss_p, ship_velocity_p, 0.1f)
          six.power = power
          val acc = (six.force / mass).x
          val (tacts, result_to) = howManyTacts(ss_p, ship_velocity_p, acc, dt)
          println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
          println("===========================")
          six.active = true
          six.worktimeTacts = tacts
        } else if(ship_velocity_p < ss_p) {
          val power = maxPossiblePower(four.force_dir.x, mass, ss_p, ship_velocity_p, 0.1f)
          four.power = power
          val acc = (four.force / mass).x
          val (tacts, result_to) = howManyTacts(ss_p, ship_velocity_p, acc, dt)
          println(s"$ship_velocity_p -> $ss_p : $tacts : $result_to : $power")
          println("===========================")
          four.active = true
          four.worktimeTacts = tacts
        }
      case None =>
    }
  }
}
