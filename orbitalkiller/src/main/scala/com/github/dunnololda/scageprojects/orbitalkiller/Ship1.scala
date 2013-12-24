package com.github.dunnololda.scageprojects.orbitalkiller

import scala.Float
import OrbitalKiller._
import com.github.dunnololda.scage.ScageLib._

class Ship1(
  val index:String,
  init_coord:Vec,
  init_velocity:Vec = Vec.zero,
  init_rotation:Float = 0f) extends Ship {
  val mass:Float = 1   // mass

  val a:Float = 50
  val b:Float = 100

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
      shape = BoxShape(a, b),
      is_static = false)
  )

  val two   = Engine(position = Vec(0, -b/2), force_dir = Vec(0, 1),  max_power = 10, power_step = 1, this)
  val eight = Engine(position = Vec(0, b/2),  force_dir = Vec(0, -1), max_power = 10, power_step = 1, this)

  val seven = Engine(position = Vec(-a/2, b/4), force_dir = Vec(1, 0),  max_power = 10, power_step = 1, this)
  val nine  = Engine(position = Vec(a/2, b/4),  force_dir = Vec(-1, 0), max_power = 10, power_step = 1, this)

  val four  = Engine(position = Vec(-a/2, 0), force_dir = Vec(1, 0),  max_power = 10, power_step = 1, this)
  val six   = Engine(position = Vec(a/2, 0),  force_dir = Vec(-1, 0), max_power = 10, power_step = 1, this)

  val one   = Engine(position = Vec(-a/2, -b/4), force_dir = Vec(1, 0),  max_power = 10, power_step = 1, this)
  val three = Engine(position = Vec(a/2, -b/4),  force_dir = Vec(-1, 0), max_power = 10, power_step = 1, this)

  val engines = List(one, two, three, four, six, seven, eight, nine)

  val engines_mapping = Map(
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD8 -> eight,

    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,

    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,

    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

  def rotateRight() {
    activateOnlyTheseEngines(seven, three)
  }

  def smallRotateRight() {
    activateOnlyOneEngine(seven)
  }

  def rotateLeft() {
    activateOnlyTheseEngines(nine, one)
  }

  def smallRotateLeft() {
    activateOnlyOneEngine(nine)
  }

  action {
    flightMode match {
      case 1 =>
      case 2 => // запрет вращения
        if(math.abs(angularVelocity*60*base_dt) < 0.01f) flightMode = 1
        else preserveAngularVelocity(0)
      case 3 => // ориентация по осям
        if(math.abs(rotation*60*base_dt) < 0.1f) flightMode = 2
        else preserveAngle(0)
      case 4 => // ориентация по траектории
        val angle = linearVelocity.deg(Vec(0,1).rotateDeg(rotation)) * math.signum(linearVelocity.perpendicular * Vec(0,1).rotateDeg(rotation))
        if(math.abs(angle) < 0.01f) flightMode = 2
        else {

        }
      case _ =>
    }
  }

  render {
    openglLocalTransform {
      openglMove(coord)
      openglRotateDeg(rotation)
      drawRectCentered(Vec.zero, a, b, color = WHITE)
      drawLine(Vec(-a/2, b/4 + b/8), Vec(a/2, b/4 + b/8), color = WHITE)

      drawEngine(two, Vec(0, -b/2-2.5f), 10, 5, is_vertical = false)

      drawEngine(eight, Vec(0, b/2+2.5f), 10, 5, is_vertical = false)

      drawEngine(seven, Vec(-a/2-2.5f, b/4), 5, 10, is_vertical = true)

      drawEngine(nine, Vec(a/2+2.5f, b/4), 5, 10, is_vertical = true)

      drawEngine(four, Vec(-a/2-2.5f, 0), 5, 10, is_vertical = true)

      drawEngine(six, Vec(a/2+2.5f, 0), 5, 10, is_vertical = true)

      drawEngine(one, Vec(-a/2-2.5f, -b/4), 5, 10, is_vertical = true)

      drawEngine(three, Vec(a/2+2.5f, -b/4), 5, 10, is_vertical = true)
    }

    drawFilledCircle(coord, 2, GREEN)                             // mass center
    drawLine(coord, coord + linearVelocity.n*100, CYAN)           // current velocity
    drawLine(coord, coord + (earth.coord - coord).n*100, YELLOW)    // direction to sun
    drawLine(coord, coord + (moon.coord - coord).n*100, GREEN)   // direction to earth
  }

  def preserveAngularVelocity(ang_vel_deg: Float) {

  }

  def enterOrbit() {

  }
}
