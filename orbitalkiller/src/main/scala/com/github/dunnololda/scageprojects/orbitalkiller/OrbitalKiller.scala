package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer

object OrbitalKiller extends ScageScreenApp("Orbital Killer", 1024, 768) {
  val G:Float = 20
  val base_dt = 0.01f
  private var _dt:Float = base_dt
  def dt = _dt

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {_dt += 0.01f; ship.updateFutureTrajectory()})
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = if(_dt > 0.01f) {_dt -= 0.01f; ship.updateFutureTrajectory()})

  /*val moon = new Star(mass = 5000, coord = Vec(-4600, 4600), radius = 200)*/
  val earth = new Star(mass = 10000, coord = Vec.zero, radius = 300)
  val ship_start_position = Vec(earth.radius*1.5f, earth.radius*1.5f)
  val ship = new Ship(a = 50, b = 100,
    init_coord = ship_start_position,
    init_velocity = ship_start_position.n.rotateDeg(90)*math.sqrt(G*earth.mass/(earth.coord.dist(ship_start_position))),
    init_rotation = math.Pi.toFloat/4
  )

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {ship.one.switchActive()})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {ship.two.switchActive()})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {ship.three.switchActive()})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {ship.four.switchActive()})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {ship.six.switchActive()})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {ship.seven.switchActive()})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {ship.eight.switchActive()})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {ship.nine.switchActive()})

  private var _center = ship.coord
  center = _center

  keyIgnorePause(KEY_W, 10, onKeyDown = _center += Vec(0, 5/globalScale))
  keyIgnorePause(KEY_A, 10, onKeyDown = _center += Vec(-5/globalScale, 0))
  keyIgnorePause(KEY_S, 10, onKeyDown = _center += Vec(0, -5/globalScale))
  keyIgnorePause(KEY_D, 10, onKeyDown = _center += Vec(5/globalScale, 0))

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.1f) {
      if(globalScale > 1) globalScale -= 1
      else globalScale -= 0.1f
    }
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else globalScale += 1
  })

  keyIgnorePause(KEY_SPACE, onKeyDown = _center = ship.coord)
  keyIgnorePause(KEY_F2, onKeyDown = {_center = center; center = _center})
  keyIgnorePause(KEY_F3, onKeyDown = center = ship.coord)
  keyIgnorePause(KEY_F4, onKeyDown = center = earth.coord)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())
  interface {
    if(onPause) print("Пауза", 20, windowHeight - 20, align = "top-left", color = WHITE)
    print("F1 - Справка", windowWidth - 20, 20, align = "bottom-right", color = GREEN)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
  }
  keyIgnorePause(KEY_F1, onKeyDown = {pause(); HelpScreen.run()})

  /*keyIgnorePause(KEY_UP, 100, onKeyDown = {ship.acceleration_time += base_dt*60; ship.updateFutureTrajectory()})
  keyIgnorePause(KEY_DOWN, 100, onKeyDown = if(ship.acceleration_time > dt) {ship.acceleration_time -= base_dt*60; ship.updateFutureTrajectory()})*/

  /*keyIgnorePause(KEY_1, onKeyDown = ship.flight_mode = 0)
  keyIgnorePause(KEY_2, onKeyDown = ship.flight_mode = 1)*/

  def systemEvolutionFrom(dt: => Float, force: (Float, BodyState, List[BodyState]) => Vec, torque: (Float, BodyState, List[BodyState]) => Float)(current_state:(Float, List[BodyState])):Stream[(Float, List[BodyState])] = {
    val (time, bodies) = current_state

    val next_time = time + dt
    val next_bodies = bodies.map { case bs =>
      val next_force = force(time, bs, bodies.filterNot(_ == bs))
      val next_acc = next_force / bs.mass
      val next_vel = bs.vel + next_acc*dt
      val next_coord = bs.coord + next_vel*dt

      val next_torque = -torque(time, bs, bodies.filterNot(_ == bs))
      val next_ang_acc = next_torque / bs.I
      val next_ang_vel = bs.ang_vel + next_ang_acc*dt
      val next_ang = (bs.ang + next_ang_vel*dt) % (2*math.Pi.toFloat)

      bs.copy(force = next_force, acc = next_acc, vel = next_vel, coord = next_coord, torque = next_torque, ang_acc= next_ang_acc, ang_vel = next_ang_vel, ang = next_ang)
    }

    val pewpew = (next_time, next_bodies)
    pewpew #:: systemEvolutionFrom(dt, force, torque)(pewpew)
  }

  pause()
}

import OrbitalKiller._

class Star(val mass:Float, val coord:Vec, val radius:Float) {
  render {
    drawCircle(coord, radius, color = WHITE)
  }
}

case class Engine(position:Vec, force:Vec, sin_angle:Float, ship:Ship) {
  val torque = force.norma*position.norma*sin_angle
  private var is_active:Boolean = false
  def isActive = is_active
  def isActive_=(bool:Boolean) {
    is_active = bool
    ship.updateFutureTrajectory()
  }
  def switchActive() {
    /*if (is_active) {is_active = false; ship.updateFutureTrajectory()}
    else if (ship.acceleration_time > 0) {is_active = true; ship.updateFutureTrajectory()}*/
    is_active = !is_active
    ship.updateFutureTrajectory()
  }
}

case class BodyState(index:String,
                     mass:Float,
                     I:Float,
                     force:Vec,
                     acc:Vec,
                     vel:Vec,
                     coord:Vec,
                     torque:Float,
                     ang_acc:Float,
                     ang_vel:Float,
                     ang:Float)

class Ship(val a:Float, val b:Float, init_coord:Vec, init_velocity:Vec = Vec.zero, init_rotation:Float = 0f) {
  val body_index = System.currentTimeMillis()+"-"+(math.random*1000).toInt

  val mass:Float = 1   // mass
  val I = mass*(a*a + b*b)/12

  private var _force = Vec.zero

  private var _linear_acceleration:Vec = Vec.zero
  def linearAcceleration = _linear_acceleration

  private var linear_velocity = init_velocity
  def linearVelocity = linear_velocity

  private var _coord = init_coord
  def coord = _coord

  private var _M = 0f

  private var _angular_acceleration:Float = 0f
  def angularAcceleration = _angular_acceleration

  private var angular_velocity:Float = 0f
  def angularVelocity = angular_velocity

  private var _rotation:Float = init_rotation
  def rotation = _rotation

  /*var acceleration_time:Float = 0f*/

  val two = Engine(position = Vec(0, -b/2), force = Vec(0, 1), sin_angle = 0, this)
  val eight = Engine(position = Vec(0, b/2), force = Vec(0, -1), sin_angle = 0, this)

  val seven = Engine(position = Vec(-a/2, b/4), force = Vec(1, 0), sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)
  val nine = Engine(position = Vec(a/2, b/4), force = Vec(-1, 0), sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)

  val four = Engine(position = Vec(-a/2, 0), force = Vec(1, 0), sin_angle = 0, this)
  val six = Engine(position = Vec(a/2, 0), force = Vec(-1, 0), sin_angle = 0, this)

  val one = Engine(position = Vec(-a/2, -b/4), force = Vec(1, 0), sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)
  val three = Engine(position = Vec(a/2, -b/4), force = Vec(-1, 0), sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)

  val engines = List(one, two, three, four, six, seven, eight, nine)

  render {
    openglLocalTransform {
      openglMove(_coord)
      openglRotateRad(_rotation)
      drawRectCentered(Vec.zero, a, b, color = WHITE)
      drawLine(Vec(-a/2, b/4 + b/8), Vec(a/2, b/4 + b/8), color = WHITE)

      drawRectCentered(Vec(0, -b/2-2.5f), 10, 5, color = (if (two.isActive) RED else WHITE))
      drawRectCentered(Vec(0, b/2+2.5f), 10, 5, color = (if (eight.isActive) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, b/4), 5, 10, color = (if (seven.isActive) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, b/4), 5, 10, color = (if (nine.isActive) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, 0), 5, 10, color = (if (four.isActive) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, 0), 5, 10, color = (if (six.isActive) RED else WHITE))

      drawRectCentered(Vec(-a/2-2.5f, -b/4), 5, 10, color = (if (one.isActive) RED else WHITE))
      drawRectCentered(Vec(a/2+2.5f, -b/4), 5, 10, color = (if (three.isActive) RED else WHITE))
    }

    drawFilledCircle(coord, 2, GREEN)                   // mass center
    drawLine(coord, coord + _force.n*20, RED)           // current force
    drawLine(coord, coord + linear_velocity.n*20, CYAN) // current velocity

    drawSlidingLines(body_trajectory, color = GREEN)
    drawSlidingLines(future_trajectory, color = YELLOW)
  }

  def currentBodyState = BodyState(body_index, mass, I, _force, _linear_acceleration, linear_velocity, _coord, _M, _angular_acceleration, angular_velocity, _rotation)
  def bodyEvolution = systemEvolutionFrom(dt,
                                          force = (time, bs, other_bodies) => {
                                            currentForce(bs)
                                            /*(if (acceleration_time > 0) engines.filter(_.isActive).foldLeft(Vec.zero) {
                                              case (sum, e) => sum + e.force
                                            }.rotateRad(bs.ang) else Vec.zero) + (earth.coord - bs.coord).n*G*bs.mass*earth.mass/earth.coord.dist2(bs.coord)*/
                                          },
                                          torque = (time, bs, other_bodies) => {
                                            currentTorque(bs)
                                            /*if (acceleration_time > 0) engines.filter(_.isActive).foldLeft(0f) {
                                              case (sum, e) => sum + e.torque
                                            } else 0f*/
                                          })((0f, List(currentBodyState))).map(x => (x._1, x._2.find(_.index == body_index).get))
  private val real_body_evolution = bodyEvolution.iterator

  def currentForce(bs:BodyState):Vec = {
    engines.filter(_.isActive).foldLeft(Vec.zero) {
      case (sum, e) => sum + e.force.rotateRad(bs.ang)
    } +
    (earth.coord - bs.coord).n*G*bs.mass*earth.mass/earth.coord.dist2(bs.coord)/* +
    (moon.coord - bs.coord).n*G*bs.mass*moon.mass/moon.coord.dist2(bs.coord)*/
  }

  def currentTorque(bs:BodyState):Float = {
    engines.filter(_.isActive).foldLeft(0f) {
      case (sum, e) => sum + e.torque
    }
  }

  private val body_trajectory = ArrayBuffer[Vec]()
  val trajectory_capacity = 100000

  private val future_trajectory = ArrayBuffer[Vec]()
  def updateFutureTrajectory() {
    /*val engines_working_time = acceleration_time*/
    future_trajectory.clear()
    future_trajectory ++= {
      val evolution = systemEvolutionFrom(dt,
                                          force = (time, bs, other_bodies) => {
                                            currentForce(bs)
                                            /*(if (time < engines_working_time) engines.filter(_.isActive).foldLeft(Vec.zero) {
                                              case (sum, e) => sum + e.force
                                            }.rotateRad(bs.ang) else Vec.zero) + (earth.coord - bs.coord).n*G*bs.mass*earth.mass/earth.coord.dist2(bs.coord)*/
                                          },
                                          torque = (time, bs, other_bodies) => {
                                            currentTorque(bs)
                                            /*if (time < engines_working_time) engines.filter(_.isActive).foldLeft(0f) {
                                              case (sum, e) => sum + e.torque
                                            } else 0f*/
                                          })((0f, List(currentBodyState)))
      evolution.take(10000).flatMap(x => x._2.filter(_.index == body_index)).map(_.coord)
    }
  }
  actionIgnorePause {
    if(future_trajectory.isEmpty) updateFutureTrajectory()
  }

  def switchEngine(e:Engine) {
    e.switchActive()
    updateFutureTrajectory()
  }

  /*var flight_mode = 0
  def flightModeStr:String = flight_mode match {
    case 1 => "Запрет вращения"
    case 2 => "Ориентация по осям"
    case 3 => "Ориентация вдоль траектории"
    case _ => "Свободное маневрирование"
  }*/

  action {
    future_trajectory.remove(0)

    val (_, BodyState(_, _, _, next_force, next_acc, next_vel, next_coord, next_torque, next_ang_acc, next_ang_vel, next_ang)) = real_body_evolution.next()

    /*if(acceleration_time > 0) {
      acceleration_time -= dt
      if(acceleration_time <= 0) {
        acceleration_time = 0f
        engines.foreach(_.isActive = false)
      }
    }*/

    _force = next_force

    _linear_acceleration = next_acc
    linear_velocity = next_vel
    _coord = next_coord

    body_trajectory += _coord
    if(body_trajectory.size >= trajectory_capacity) body_trajectory.remove(0, 1000)

    _M = next_torque

    _angular_acceleration = next_ang_acc
    angular_velocity = next_ang_vel
    _rotation = next_ang

    /*flight_mode match {
      case 1 =>
        if(angular_velocity > 0) {
          seven.isActive = true
          one.isActive = false
          nine.isActive = false
          three.isActive = false
        }
        else if(angular_velocity < 0) {
          seven.isActive = false
          one.isActive = false
          nine.isActive = true
          three.isActive = false
        }
      case _ =>
    }*/
  }

  interface {
    print(s"Ускорение времени: x${(dt/0.01f).toInt}", 20, 140, ORANGE)
    /*print(f"Время работы двигателей: ${acceleration_time/base_dt/60}%.0f сек", 20, 120, ORANGE)*/
    /*print(s"Режим: ${flightModeStr}", 20, 100, ORANGE)*/
    print(f"Позиция: ${coord.x}%.2f : ${coord.y}%.2f", 20, 80, ORANGE)
    print(f"Угловая скорость: ${angular_velocity/math.Pi*180*60*base_dt}%.2f град/сек", 20, 60, ORANGE)
    print(f"Скорость: ${linear_velocity.norma*60*base_dt}%.2f м/сек ( velx = ${linear_velocity.x*60*base_dt}%.2f м/сек, vely = ${linear_velocity.y*60*base_dt}%.2f м/сек)", 20, 40, ORANGE)
    print(s"Угол: ${(_rotation/math.Pi*180).toInt} град", 20, 20, ORANGE)
    /*print("F = "+_force, 20, 40, WHITE)
    print("M = "+_M, 20, 20, WHITE)*/
  }
}
