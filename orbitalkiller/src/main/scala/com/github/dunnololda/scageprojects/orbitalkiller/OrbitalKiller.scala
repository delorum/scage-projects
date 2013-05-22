package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._

object OrbitalKiller extends ScageScreenApp("Orbital Killer", 800, 600) {
  val ship = new RectangleBody(50, 100, windowCenter, math.Pi.toFloat/4)

  key(KEY_NUMPAD1, onKeyDown = ship.one.is_active = true, onKeyUp = ship.one.is_active = false)
  key(KEY_NUMPAD2, onKeyDown = ship.two.is_active = true, onKeyUp = ship.two.is_active = false)
  key(KEY_NUMPAD3, onKeyDown = ship.three.is_active = true, onKeyUp = ship.three.is_active = false)
  key(KEY_NUMPAD4, onKeyDown = ship.four.is_active = true, onKeyUp = ship.four.is_active = false)
  key(KEY_NUMPAD5, onKeyDown = ship.stopRotation())
  key(KEY_NUMPAD6, onKeyDown = ship.six.is_active = true, onKeyUp = ship.six.is_active = false)
  key(KEY_NUMPAD7, onKeyDown = ship.seven.is_active = true, onKeyUp = ship.seven.is_active = false)
  key(KEY_NUMPAD8, onKeyDown = ship.eight.is_active = true, onKeyUp = ship.eight.is_active = false)
  key(KEY_NUMPAD9, onKeyDown = ship.nine.is_active = true, onKeyUp = ship.nine.is_active = false)

  val dt:Float = 0.01f
}

import OrbitalKiller._

case class Engine(position:Vec, force:Vec, sin_angle:Float) {

  var is_active:Boolean = false
}

class RectangleBody(val a:Float, val b:Float, val init_coord:Vec, val init_rotation:Float = 0f) {
  val mass:Float = 1   // mass
  val I = mass*(a*a + b*b)/12

  render {
    openglLocalTransform {
      openglMove(_coord)
      openglRotateRad(_rotation)
      drawRectCentered(Vec.zero, a, b, color = WHITE)

      drawRectCentered(Vec(0, -b/2-2.5f), 10, 5, color = (if (two.is_active) RED else WHITE))
      drawRectCentered(Vec(0, b/2+2.5f), 10, 5, color = (if (eight.is_active) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, b/4), 5, 10, color = (if (seven.is_active) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, b/4), 5, 10, color = (if (nine.is_active) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, 0), 5, 10, color = (if (four.is_active) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, 0), 5, 10, color = (if (six.is_active) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, -b/4), 5, 10, color = (if (one.is_active) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, -b/4), 5, 10, color = (if (three.is_active) RED else WHITE))
    }

    drawFilledCircle(coord, 2, GREEN)       // mass center
    drawLine(coord, coord + _force.n*20, RED) // current force
    drawLine(coord, coord + _linear_velocity.n*20, CYAN) // current velocity
  }

  private var _linear_acceleration:Vec = Vec.zero
  def linearAcceleration = _linear_acceleration

  private var _linear_velocity = Vec.zero
  def linearVelocity = _linear_velocity

  private var _coord = init_coord
  def coord = _coord

  private var _angular_acceleration:Float = 0f
  def angularAcceleration = _angular_acceleration

  private var _angular_velocity:Float = 0f
  def angularVelocity = _angular_velocity

  private var _rotation:Float = init_rotation
  def rotation = _rotation

  def stopRotation() {_angular_velocity = 0f}

  val two = Engine(position = Vec(0, -b/2), force = Vec(0, 1), sin_angle = 0)
  val eight = Engine(position = Vec(0, b/2), force = Vec(0, -1), sin_angle = 0)

  val seven = Engine(position = Vec(-a/2, b/4), force = Vec(1, 0), sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat)
  val nine = Engine(position = Vec(a/2, b/4), force = Vec(-1, 0), sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat)

  val four = Engine(position = Vec(-a/2, 0), force = Vec(1, 0), sin_angle = 0)
  val six = Engine(position = Vec(a/2, 0), force = Vec(-1, 0), sin_angle = 0)

  val one = Engine(position = Vec(-a/2, -b/4), force = Vec(1, 0), sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat)
  val three = Engine(position = Vec(a/2, -b/4), force = Vec(-1, 0), sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat)

  val engines = List(one, two, three, four, six, seven, eight, nine)

  private var _force = Vec.zero
  private var _M = 0f

  action {
    val active_engines = engines.filter(_.is_active)
    val forces_and_rs_and_angles = active_engines.map(e => {
      (e.force.rotateRad(_rotation), (Vec.zero - e.position).rotateRad(_rotation), e.sin_angle)
    })

    val force = forces_and_rs_and_angles.foldLeft(Vec.zero) {
      case (sum, (f, _, _)) => sum + f
    }

    _force = force

    _linear_acceleration = force / mass
    _linear_velocity += _linear_acceleration*dt
    _coord += _linear_velocity*dt

    val M = forces_and_rs_and_angles.foldLeft(0f) {
      case (res, (f, r, sa)) => res + (f.norma * r.norma)*sa
    }

    _M = M

    _angular_acceleration = -M / I
    _angular_velocity += _angular_acceleration*dt
    _rotation += _angular_velocity*dt
    _rotation %= (2*math.Pi).toFloat
  }

  interface {
    print("ang vel = "+_angular_velocity, 20, 60, WHITE)
    print("vel = "+_linear_velocity.norma+" ( velx = "+_linear_velocity.x+", vely = "+_linear_velocity.y+")", 20, 40, WHITE)
    print("ang = "+(_rotation/math.Pi*180).toInt, 20, 20, WHITE)
    /*print("F = "+_force, 20, 40, WHITE)
    print("M = "+_M, 20, 20, WHITE)*/
  }
}
