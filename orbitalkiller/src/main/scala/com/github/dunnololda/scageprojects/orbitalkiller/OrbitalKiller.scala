package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer

object OrbitalKiller extends ScageScreenApp("Orbital Killer", 1024, 768) {
  val ship = new RectangleBody(50, 100, windowCenter, math.Pi.toFloat/4)

  key(KEY_NUMPAD1, onKeyDown = {ship.one.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD2, onKeyDown = {ship.two.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD3, onKeyDown = {ship.three.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD4, onKeyDown = {ship.four.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD5, onKeyDown = {ship.stopRotation(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD6, onKeyDown = {ship.six.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD7, onKeyDown = {ship.seven.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD8, onKeyDown = {ship.eight.switchActive(); ship.updateFutureTrajectory()})
  key(KEY_NUMPAD9, onKeyDown = {ship.nine.switchActive(); ship.updateFutureTrajectory()})

  private var _center = windowCenter
  center = _center

  keyIgnorePause(KEY_W, 10, onKeyDown = _center += Vec(0, 5/globalScale))
  keyIgnorePause(KEY_A, 10, onKeyDown = _center += Vec(-5/globalScale, 0))
  keyIgnorePause(KEY_S, 10, onKeyDown = _center += Vec(0, -5/globalScale))
  keyIgnorePause(KEY_D, 10, onKeyDown = _center += Vec(5/globalScale, 0))

  mouseWheelDownIgnorePause(onWheelDown = m => if(globalScale > 1) globalScale -= 1)
  mouseWheelUpIgnorePause(onWheelUp = m => globalScale += 1)

  keyIgnorePause(KEY_SPACE, onKeyDown = _center = ship.coord)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())
  interface {
    if(onPause) print("Пауза", 20, windowHeight - 30, color = WHITE)
    print("F1 - Справка", windowWidth - 20, 20, align = "bottom-right", color = GREEN)
  }
  keyIgnorePause(KEY_F1, onKeyDown = {pause(); HelpScreen.run()})

  val dt:Float = 0.01f
}

import OrbitalKiller._

case class Engine(position:Vec, force:Vec, sin_angle:Float) {
  val torque = force.norma*position.norma*sin_angle
  var is_active:Boolean = false
  def switchActive() {is_active = !is_active}
}

case class BodyState(vel:Vec, coord:Vec, ang_vel:Float, ang:Float)

class RectangleBody(val a:Float, val b:Float, val init_coord:Vec, val init_rotation:Float = 0f) {
  val mass:Float = 1   // mass
  val I = mass*(a*a + b*b)/12

  render {
    openglLocalTransform {
      openglMove(_coord)
      openglRotateRad(_rotation)
      drawRectCentered(Vec.zero, a, b, color = WHITE)
      drawLine(Vec(-a/2, b/4 + b/8), Vec(a/2, b/4 + b/8), color = WHITE)

      drawRectCentered(Vec(0, -b/2-2.5f), 10, 5, color = (if (two.is_active) RED else WHITE))
      drawRectCentered(Vec(0, b/2+2.5f), 10, 5, color = (if (eight.is_active) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, b/4), 5, 10, color = (if (seven.is_active) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, b/4), 5, 10, color = (if (nine.is_active) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, 0), 5, 10, color = (if (four.is_active) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, 0), 5, 10, color = (if (six.is_active) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, -b/4), 5, 10, color = (if (one.is_active) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, -b/4), 5, 10, color = (if (three.is_active) RED else WHITE))
    }

    drawFilledCircle(coord, 2, GREEN)                   // mass center
    drawLine(coord, coord + _force.n*20, RED)           // current force
    drawLine(coord, coord + linear_velocity.n*20, CYAN) // current velocity

    drawSlidingLines(body_trajectory, color = GREEN)
    drawSlidingLines(future_trajectory, color = YELLOW)
  }

  def bodyStateFrom(bp:BodyState):Stream[BodyState] = {
    val force = currentForce(bp.ang)
    val M = currentTorque(bp.ang)
    val next_acc = force / mass
    val next_vel = bp.vel + next_acc*dt
    val next_coord = bp.coord + next_vel*dt
    val ang_acc = -M / I
    val next_ang_vel = bp.ang_vel + ang_acc*dt
    val next_ang = (bp.ang + next_ang_vel*dt) % (2*math.Pi.toFloat)
    val next_bp = BodyState(next_vel, next_coord, next_ang_vel, next_ang)
    next_bp #:: bodyStateFrom(next_bp)
  }

  def currentBodyState = BodyState(linear_velocity, _coord, angular_velocity, _rotation)

  def currentForce(rotation:Float):Vec = {
    engines.filter(_.is_active).foldLeft(Vec.zero) {
      case (sum, e) => sum + e.force.rotateRad(rotation)
    }
  }

  def currentTorque(rotation:Float):Float = {
    engines.filter(_.is_active).foldLeft(0f) {
      case (sum, e) => sum + e.torque
    }
  }

  private val body_trajectory = ArrayBuffer[Vec]()
  val trajectory_capacity = 100000

  private val future_trajectory = ArrayBuffer[Vec]()
  def updateFutureTrajectory() {
    future_trajectory.clear()
    future_trajectory ++= bodyStateFrom(currentBodyState).take(10000).map(_.coord)
  }
  action {
    if(future_trajectory.isEmpty) updateFutureTrajectory()
    future_trajectory.remove(0)
  }

  def switchEngine(e:Engine) {
    e.switchActive()
    updateFutureTrajectory()
  }

  private var _linear_acceleration:Vec = Vec.zero
  def linearAcceleration = _linear_acceleration

  private var linear_velocity = Vec.zero
  def linearVelocity = linear_velocity

  private var _coord = init_coord
  def coord = _coord

  private var _angular_acceleration:Float = 0f
  def angularAcceleration = _angular_acceleration

  private var angular_velocity:Float = 0f
  def angularVelocity = angular_velocity

  private var _rotation:Float = init_rotation
  def rotation = _rotation

  def stopRotation() {angular_velocity = 0f}

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
    linear_velocity += _linear_acceleration*dt
    _coord += linear_velocity*dt

    body_trajectory += _coord
    if(body_trajectory.size >= trajectory_capacity) body_trajectory.remove(0, 1000)

    val M = forces_and_rs_and_angles.foldLeft(0f) {
      case (res, (f, r, sa)) => res + (f.norma * r.norma)*sa
    }

    _M = M

    _angular_acceleration = -M / I
    angular_velocity += _angular_acceleration*dt
    _rotation += angular_velocity*dt
    _rotation %= (2*math.Pi).toFloat
  }

  interface {
    print(f"Позиция: ${coord.x}%.2f : ${coord.y}%.2f", 20, 80, ORANGE)
    print(f"Угловая скорость: ${angular_velocity/math.Pi*180*60*dt}%.2f град/сек", 20, 60, ORANGE)
    print(f"Скорость: ${linear_velocity.norma*60*dt}%.2f м/сек ( velx = ${linear_velocity.x*60*dt}%.2f м/сек, vely = ${linear_velocity.y*60*dt}%.2f м/сек)", 20, 40, ORANGE)
    print(s"Угол: ${(_rotation/math.Pi*180).toInt} град", 20, 20, ORANGE)
    /*print("F = "+_force, 20, 40, WHITE)
    print("M = "+_M, 20, 20, WHITE)*/
  }
}
