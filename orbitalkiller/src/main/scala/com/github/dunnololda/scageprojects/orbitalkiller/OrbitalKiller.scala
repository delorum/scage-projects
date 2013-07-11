package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer

object OrbitalKiller extends ScageScreenApp("Orbital Killer", 640, 480) {
  val G:Float = 20
  val base_dt = 0.01f
  private var time_mulitplier = 1
  private var _dt:Float = base_dt
  def dt = _dt

  /*val moon = new Star(mass = 5000, coord = Vec(-4600, 4600), radius = 200)*/
  val earth = new Star(mass = 10000, coord = Vec.zero, radius = 300)
  val ship_start_position = Vec(earth.radius*1.5f, earth.radius*1.5f)
  val ship = new Ship(a = 50, b = 100,
    init_coord = ship_start_position,
    init_velocity = ship_start_position.n.rotateDeg(90)*math.sqrt(G*earth.mass/earth.coord.dist(ship_start_position)),
    init_rotation = math.Pi.toFloat/4
  )

  private var view_mode = 0
  def viewMode = view_mode
  def viewMode_=(new_view_mode:Int) {
    new_view_mode match {
      case 0 =>
        _center = center
        center = _center
        rotationAngle = 0
        view_mode = 0
      case 1 =>
        center = ship.coord
        rotationPoint = ship.coord
        rotationAngleRad = -ship.rotation
        view_mode = 1
      case 2 =>
        center = earth.coord
        rotationAngle = 0
        view_mode = 2
      case _ =>
    }
  }
  private def viewModeStr = view_mode match {
    case 0 => "свободный"
    case 1 => "фиксация на корабле"
    case 2 => "фиксация на планете"
    case _ => ""
  }

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

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {time_mulitplier += 1; _dt = time_mulitplier*base_dt; ship.updateFutureTrajectory()})
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = if(time_mulitplier > 1) {time_mulitplier -= 1; _dt = time_mulitplier*base_dt; ship.updateFutureTrajectory()})

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {ship.one.switchActive()})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {ship.two.switchActive()})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {ship.three.switchActive()})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {ship.four.switchActive()})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {ship.six.switchActive()})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {ship.seven.switchActive()})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {ship.eight.switchActive()})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {ship.nine.switchActive()})

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {ship.engines.foreach(_.power = 1f)})

  keyIgnorePause(KEY_UP,   10, onKeyDown = {ship.engines.filter(_.active).foreach(_.power += 0.1f)})
  keyIgnorePause(KEY_DOWN, 10, onKeyDown = {ship.engines.filter(_.active).foreach(_.power -= 0.1f)})

  keyIgnorePause(KEY_W, 10, onKeyDown = {viewMode = 0; _center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {viewMode = 0; _center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {viewMode = 0; _center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {viewMode = 0; _center += Vec(5/globalScale, 0)})

  keyIgnorePause(KEY_SPACE, onKeyDown = _center = ship.coord)
  keyIgnorePause(KEY_F2, onKeyDown = viewMode = 0)
  keyIgnorePause(KEY_F3, onKeyDown = viewMode = 1)
  keyIgnorePause(KEY_F4, onKeyDown = viewMode = 2)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {pause(); HelpScreen.run()})

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01f) {
      if(globalScale > 1) globalScale -= 1
      else if(globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 5) {
      if(globalScale < 1) globalScale += 0.1f
      else globalScale += 1
    }
    println(globalScale)
  })

  private var _center = ship.coord
  center = _center

  render {
    val y = windowHeight/2-20
    val x = windowWidth/2-20
    val from1 = center + Vec(0, -y/globalScale)
    val to1 = center + Vec(0, y/globalScale)
    val arrow11 = to1 + ((from1-to1).n*10/globalScale).rotateDeg(15)
    val arrow12 = to1 + ((from1-to1).n*10/globalScale).rotateDeg(-15)
    drawLine(from1, to1, DARK_GRAY)
    drawLine(to1, arrow11, DARK_GRAY)
    drawLine(to1, arrow12, DARK_GRAY)

    val from2 = center + Vec(-x/globalScale, 0)
    val to2 = center + Vec(x/globalScale, 0)
    val arrow21 = to2 + ((from2-to2).n*10/globalScale).rotateDeg(15)
    val arrow22 = to2 + ((from2-to2).n*10/globalScale).rotateDeg(-15)
    drawLine(from2, to2, DARK_GRAY)
    drawLine(to2, arrow21, DARK_GRAY)
    drawLine(to2, arrow22, DARK_GRAY)
  }

  interface {
    print(s"Ускорение времени: x$time_mulitplier", 20, 140, ORANGE)
    print(fps, 20, windowHeight - 20, align = "top-left", color = DARK_GRAY)
    if(onPause) print("Пауза", windowCenter, align = "center", color = WHITE)
    print(s"Режим камеры: $viewModeStr", 20, 100, ORANGE)
    print("F1 - Справка, F2, F3, F4 - режимы камеры", windowWidth - 20, 20, align = "bottom-right", color = GREEN)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
  }

  pause()
}

import OrbitalKiller._

class Star(val mass:Float, val coord:Vec, val radius:Float) {
  render {
    drawCircle(coord, radius, color = WHITE)
  }
}

case class Engine(position:Vec, force_dir:Vec, max_power:Float, sin_angle:Float, ship:Ship) {
  private var _power:Float = 1f
  def power = _power
  def power_=(new_power:Float) {
    if(new_power >= 1f && new_power < max_power && new_power != _power) {
      _power = new_power
      ship.updateFutureTrajectory()
    }
  }

  def force = force_dir*power
  def torque = force.norma*position.norma*sin_angle
  private var is_active:Boolean = false
  def active = is_active
  def active_=(bool:Boolean) {
    if(is_active != bool) {
      is_active = bool
      if(!is_active) _power = 1f
      ship.updateFutureTrajectory()
    }
  }
  def switchActive() {
    is_active = !is_active
    if(!is_active) _power = 1f
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

  val two   = Engine(position = Vec(0, -b/2), force_dir = Vec(0, 1),  max_power = 10, sin_angle = 0, this)
  val eight = Engine(position = Vec(0, b/2),  force_dir = Vec(0, -1), max_power = 10, sin_angle = 0, this)

  val seven = Engine(position = Vec(-a/2, b/4), force_dir = Vec(1, 0),  max_power = 10, sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat,  this)
  val nine  = Engine(position = Vec(a/2, b/4),  force_dir = Vec(-1, 0), max_power = 10, sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)

  val four  = Engine(position = Vec(-a/2, 0), force_dir = Vec(1, 0),  max_power = 10, sin_angle = 0, this)
  val six   = Engine(position = Vec(a/2, 0),  force_dir = Vec(-1, 0), max_power = 10, sin_angle = 0, this)

  val one   = Engine(position = Vec(-a/2, -b/4), force_dir = Vec(1, 0),  max_power = 10, sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)
  val three = Engine(position = Vec(a/2, -b/4),  force_dir = Vec(-1, 0), max_power = 10, sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat,  this)

  val engines = List(one, two, three, four, six, seven, eight, nine)

  def currentBodyState = BodyState(body_index, mass, I, _force, _linear_acceleration, linear_velocity, _coord, _M, _angular_acceleration, angular_velocity, _rotation)
  def bodyEvolution = systemEvolutionFrom(dt,
                                          force = (time, bs, other_bodies) => {
                                            currentForce(bs)
                                          },
                                          torque = (time, bs, other_bodies) => {
                                            currentTorque(bs)
                                          })((0f, List(currentBodyState))).map(x => (x._1, x._2.find(_.index == body_index).get))
  private val real_body_evolution = bodyEvolution.iterator

  def currentForce(bs:BodyState):Vec = {
    engines.filter(_.active).foldLeft(Vec.zero) {
      case (sum, e) => sum + e.force.rotateRad(bs.ang)
    } +
    (earth.coord - bs.coord).n*G*bs.mass*earth.mass/earth.coord.dist2(bs.coord)/* +
    (moon.coord - bs.coord).n*G*bs.mass*moon.mass/moon.coord.dist2(bs.coord)*/
  }

  def currentTorque(bs:BodyState):Float = {
    engines.filter(_.active).foldLeft(0f) {
      case (sum, e) => sum + e.torque
    }
  }

  private val body_trajectory = ArrayBuffer[Vec]()
  val trajectory_capacity = 100000

  private val future_trajectory = ArrayBuffer[Vec]()
  def updateFutureTrajectory() {
    future_trajectory.clear()
    future_trajectory ++= {
      val evolution = systemEvolutionFrom(dt,
                                          force = (time, bs, other_bodies) => {
                                            currentForce(bs)
                                          },
                                          torque = (time, bs, other_bodies) => {
                                            currentTorque(bs)
                                          })((0f, List(currentBodyState)))
      evolution.take(10000).flatMap(x => x._2.filter(_.index == body_index)).map(_.coord)
    }
  }

  def switchEngine(e:Engine) {
    e.switchActive()
    updateFutureTrajectory()
  }

  def engineColor(e:Engine):ScageColor = {
    if(e.active) RED else WHITE
  }

  def engineActiveWidth(e:Engine):Float = {
    10f*e.power/e.max_power
  }

  actionIgnorePause {
    if(future_trajectory.isEmpty) updateFutureTrajectory()
  }

  action {
    future_trajectory.remove(0)

    val (_, BodyState(_, _, _, next_force, next_acc, next_vel, next_coord, next_torque, next_ang_acc, next_ang_vel, next_ang)) = real_body_evolution.next()

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
  }

  render {
    openglLocalTransform {
      openglMove(_coord)
      openglRotateRad(_rotation)
      drawRectCentered(Vec.zero, a, b, color = WHITE)
      drawLine(Vec(-a/2, b/4 + b/8), Vec(a/2, b/4 + b/8), color = WHITE)

      drawRectCentered(Vec(0, -b/2-2.5f), 10, 5, color = engineColor(two))
      if(two.active && two.power > 0) drawFilledRectCentered(Vec(0, -b/2-2.5f), engineActiveWidth(two), 5, color = engineColor(two))

      drawRectCentered(Vec(0, b/2+2.5f),  10, 5, color = engineColor(eight))
      if(eight.active && eight.power > 0) drawFilledRectCentered(Vec(0, b/2+2.5f),  engineActiveWidth(eight), 5, color = engineColor(eight))

      drawRectCentered(Vec(-a/2-2.5f, b/4), 5, 10, color = engineColor(seven))
      if(seven.active && seven.power > 0) drawFilledRectCentered(Vec(-a/2-2.5f, b/4), 5, engineActiveWidth(seven), color = engineColor(seven))

      drawRectCentered(Vec(a/2+2.5f, b/4),  5, 10, color = engineColor(nine))
      if(nine.active && nine.power > 0) drawFilledRectCentered(Vec(a/2+2.5f, b/4),  5, engineActiveWidth(nine), color = engineColor(nine))

      drawRectCentered(Vec(-a/2-2.5f, 0), 5, 10, color = engineColor(four))
      if(four.active && four.power > 0) drawFilledRectCentered(Vec(-a/2-2.5f, 0), 5, engineActiveWidth(four), color = engineColor(four))

      drawRectCentered(Vec(a/2+2.5f, 0),  5, 10, color = engineColor(six))
      if(six.active && six.power > 0) drawFilledRectCentered(Vec(a/2+2.5f, 0),  5, engineActiveWidth(six), color = engineColor(six))

      drawRectCentered(Vec(-a/2-2.5f, -b/4), 5, 10, color = engineColor(one))
      if(one.active && one.power > 0) drawFilledRectCentered(Vec(-a/2-2.5f, -b/4), 5, engineActiveWidth(one), color = engineColor(one))

      drawRectCentered(Vec(a/2+2.5f, -b/4),  5, 10, color = engineColor(three))
      if(three.active && three.power > 0) drawFilledRectCentered(Vec(a/2+2.5f, -b/4),  5, engineActiveWidth(three), color = engineColor(three))
    }

    drawFilledCircle(coord, 2, GREEN)                   // mass center
    drawLine(coord, coord + _force.n*20, RED)           // current force
    drawLine(coord, coord + linear_velocity.n*20, CYAN) // current velocity

    drawSlidingLines(body_trajectory, color = GREEN)
    drawSlidingLines(future_trajectory, color = YELLOW)
  }

  interface {
    print(f"Позиция: ${coord.x}%.2f : ${coord.y}%.2f", 20, 80, ORANGE)
    print(f"Угловая скорость: ${angular_velocity/math.Pi*180*60*base_dt}%.2f град/сек", 20, 60, ORANGE)
    print(f"Скорость: ${linear_velocity.norma*60*base_dt}%.2f м/сек ( velx = ${linear_velocity.x*60*base_dt}%.2f м/сек, vely = ${linear_velocity.y*60*base_dt}%.2f м/сек)", 20, 40, ORANGE)
    print(s"Угол: ${(_rotation/math.Pi*180).toInt} град", 20, 20, ORANGE)
    /*print("F = "+_force, 20, 40, WHITE)
    print("M = "+_M, 20, 20, WHITE)*/
  }
}
