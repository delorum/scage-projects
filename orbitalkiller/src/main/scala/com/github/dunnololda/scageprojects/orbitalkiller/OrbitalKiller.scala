package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer
import scala.collection.mutable

object OrbitalKiller extends ScageScreenApp("Orbital Killer", 1024, 768) {
  val G:Float = 20
  val base_dt = 0.01f // 1/60 секунды
  private var _time_mulitplier = 1
  def timeMultiplier = _time_mulitplier
  private var _dt:Float = base_dt
  def dt = _dt

  private var _time = 0l   // tacts
  def time = _time

  private def timeStr(time_sec:Long):String = {
    val sec  = 1l
    val min  = sec*60
    val hour  = min*60
    val day  = hour*24
    s"${time_sec/day} дн ${time_sec%day/hour} ч ${time_sec%hour/min} мин ${time_sec%min/sec} сек"
  }

  def systemEvolutionFrom(dt: => Float,
                          force: (Long, BodyState, List[BodyState]) => Vec,
                          torque: (Long, BodyState, List[BodyState]) => Float)
                         (current_state:(Long, List[BodyState])):Stream[(Long, List[BodyState])] = {
    val (time, bodies) = current_state

    val next_time = time + (dt/base_dt).toLong
    val next_bodies = bodies.map { case bs =>
      val next_force = force(time, bs, bodies.filterNot(_ == bs))
      val next_acc = next_force / bs.mass
      val next_vel = bs.vel + next_acc*dt
      val next_coord = bs.coord + next_vel*dt

      val next_torque = -torque(time, bs, bodies.filterNot(_ == bs))
      val next_ang_acc = (next_torque / bs.I)/math.Pi.toFloat*180f  // in degrees
      val next_ang_vel = bs.ang_vel + next_ang_acc*dt
      val next_ang = (bs.ang + next_ang_vel*dt) % 360f

      bs.copy(force = next_force,
              acc = next_acc,
              vel = next_vel,
              coord = next_coord,
              torque = next_torque,
              ang_acc= next_ang_acc,
              ang_vel = next_ang_vel,
              ang = next_ang)
    }

    val pewpew = (next_time, next_bodies)
    pewpew #:: systemEvolutionFrom(dt, force, torque)(pewpew)
  }

  def gravityForce(body1_coord:Vec, body1_mass:Float, body2_coord:Vec, body2_mass:Float):Vec = {
    (body1_coord - body2_coord).n*G*body1_mass*body2_mass/body1_coord.dist2(body2_coord)
  }

  def satelliteSpeed(from_planet_to_body:Vec, planet_mass:Float):Vec = {
    from_planet_to_body.n.rotateDeg(90)*math.sqrt(G*planet_mass/from_planet_to_body.norma)
  }

  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    dt,
    force = (time, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          gravityForce(sun.coord, sun.mass, bs.coord, bs.mass) +
          other_bodies.filter(_.index == "earth").map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass)).sum +
          ship.currentReactiveForce(time, bs)
        case earth.index =>
          gravityForce(sun.coord, sun.mass, bs.coord, bs.mass)
        case _ => Vec.zero
      }
    },
    torque = (time, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          ship.currentTorque(time, bs)
        case _ =>
          0f
      }
    })((time, body_states))

  val trajectory_accuracy = 100
  val trajectory_capacity = 100000

  private val future_trajectory = ArrayBuffer[(Long, List[BodyState])]()
  private val future_trajectory_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()
  private def future_trajectory_capacity = if(ship.flightMode == 1) 10000 else 100

  private def updateFutureTrajectoryMap() {
    future_trajectory_map.clear()
    future_trajectory.foreach {
      case (time_moment, body_states) =>
        body_states.foreach {
          case bs =>
            future_trajectory_map.getOrElseUpdate(bs.index, ArrayBuffer[(Long, BodyState)]()) += (time_moment -> bs)
        }
    }
  }

  def updateFutureTrajectory() {
    future_trajectory.clear()
    future_trajectory ++= {
      futureSystemEvolutionFrom(time, currentBodyStates)
        .take(future_trajectory_capacity)
        .zipWithIndex
        .filter(_._2 % trajectory_accuracy == 0)
        .map(_._1)
    }
    updateFutureTrajectoryMap()
  }

  private def continueFutureTrajectoryMap(steps:Seq[(Long, List[BodyState])]) {
    steps.foreach {
      case (time_moment, body_states) =>
        body_states.foreach {
          case bs =>
            future_trajectory_map.getOrElseUpdate(bs.index, ArrayBuffer[(Long, BodyState)]()) += (time_moment -> bs)
        }
    }
  }

  def continueFutureTrajectory() {
    val (t, s) = future_trajectory.lastOption.getOrElse((time, currentBodyStates))
    val steps = {
      futureSystemEvolutionFrom(t, s)
        .take(future_trajectory_capacity)
        .zipWithIndex
        .filter(_._2 % trajectory_accuracy == 0)
        .map(_._1)
    }.toSeq
    future_trajectory ++= steps
    continueFutureTrajectoryMap(steps)
  }

  val sun = new Star(mass = 10000, coord = Vec.zero, radius = 3000)
  val earth_start_position = Vec(-sun.radius*20f, sun.radius*20f)
  val earth_init_velocity = satelliteSpeed(earth_start_position, sun.mass)
  val earth = new Planet(
    "earth",
    mass = 1000,
    init_coord = earth_start_position,
    init_velocity = earth_init_velocity,
    radius = 1000)

  val ship_start_position = earth_start_position + Vec(earth.radius*1.5f, earth.radius*1.5f)
  val ship_init_velocity = satelliteSpeed(ship_start_position - earth_start_position, earth.mass)
  val ship = new Ship(a = 50, b = 100,
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 45
  )

  private val real_system_evolution =
    futureSystemEvolutionFrom(0, List(ship.currentState, earth.currentState)).iterator

  private def nextStep() {
    val (t, body_states) = real_system_evolution.next()
    _time = t
    body_states.foreach {
      case bs => current_body_states(bs.index) = bs
    }
  }
  nextStep()

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
        rotationAngleDeg = -ship.rotation
        view_mode = 1
      case 2 =>
        center = sun.coord
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

  def satelliteSpeedInPoint(coord:Vec, speed:Vec):String = {
    if(coord.dist(earth.coord) < 3*earth.radius) {
      val ss = satelliteSpeed(coord - earth.coord, earth.mass)
      val d = ss - speed
      f"(velx = ${ss.x*60*base_dt}%.2f м/сек, vely = ${ss.y*60*base_dt}%.2f м/сек), разница: (velx = ${d.x*60*base_dt}%.2f м/сек, vely = ${d.y*60*base_dt}%.2f м/сек)"
    } else if(coord.dist(sun.coord) < 3*sun.radius) {
      val ss = satelliteSpeed(coord - sun.coord, sun.mass)
      val d = ss - speed
      f"(velx = ${ss.x*60*base_dt}%.2f м/сек, vely = ${ss.y*60*base_dt}%.2f м/сек), разница: (velx = ${d.x*60*base_dt}%.2f м/сек, vely = ${d.y*60*base_dt}%.2f м/сек)"
    } else "N/A"
  }

  def linearSpeedWhenEnginesOff:String = {
    future_trajectory.find(_._1 >= ship.engines_stop_moment_seconds) match {
      case Some((t, lbs)) =>
        lbs.find(_.index == ship.index) match {
          case Some(bs) =>
            val s = bs.vel
            f"${s.norma*60*base_dt}%.2f м/сек ( velx = ${s.x*60*base_dt}%.2f м/сек, vely = ${s.y*60*base_dt}%.2f м/сек)"
          case None => "N/A"
        }
      case None => "N/A"
    }
  }

  def angularSpeedWhenEnginesOff:String = {
    future_trajectory.find(_._1 >= ship.engines_stop_moment_seconds) match {
      case Some((t, lbs)) =>
        lbs.find(_.index == ship.index) match {
          case Some(bs) =>
            val s = bs.ang_vel
            f" ${s*60*base_dt}%.2f град/сек"
          case None => "N/A"
        }
      case None => "N/A"
    }
  }

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {ship.one.switchActive()})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {ship.two.switchActive()})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {ship.three.switchActive()})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {ship.four.switchActive()})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {ship.six.switchActive()})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {ship.seven.switchActive()})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {ship.eight.switchActive()})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {ship.nine.switchActive()})

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {ship.engines.foreach(_.active = false)})

  keyIgnorePause(KEY_UP,   10, onKeyDown = {ship.engines.filter(_.active).foreach(_.power += 0.1f)})
  keyIgnorePause(KEY_DOWN, 10, onKeyDown = {ship.engines.filter(_.active).foreach(_.power -= 0.1f)})
  keyIgnorePause(KEY_RIGHT,   10, onKeyDown = {
    ship.engines_worktime_tacts += 1
    ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*_time_mulitplier
    updateFutureTrajectory()
  })
  keyIgnorePause(KEY_LEFT, 10, onKeyDown = {
    if(ship.engines_worktime_tacts > 0) {
      ship.engines_worktime_tacts -= 1
      ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*_time_mulitplier
      updateFutureTrajectory()
    }
  })

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    _time_mulitplier += 1
    _dt = _time_mulitplier*base_dt
    ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*_time_mulitplier
    updateFutureTrajectory()
  })
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    if(_time_mulitplier > 1) {
      _time_mulitplier -= 1
      _dt = _time_mulitplier*base_dt
      ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*_time_mulitplier
      updateFutureTrajectory()
    }
  })

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    _time_mulitplier += 40
    _dt = _time_mulitplier*base_dt
    ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*_time_mulitplier
    updateFutureTrajectory()
  })
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    if (_time_mulitplier != 1) {
      _time_mulitplier = 1
      _dt = _time_mulitplier*base_dt
      ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*_time_mulitplier
      updateFutureTrajectory()
    }
  })

  keyIgnorePause(KEY_W, 10, onKeyDown = {viewMode = 0; _center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {viewMode = 0; _center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {viewMode = 0; _center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {viewMode = 0; _center += Vec(5/globalScale, 0)})

  keyIgnorePause(KEY_SPACE, onKeyDown = _center = ship.coord)
  keyIgnorePause(KEY_F2, onKeyDown = viewMode = 0)
  keyIgnorePause(KEY_F3, onKeyDown = viewMode = 1)
  keyIgnorePause(KEY_F4, onKeyDown = viewMode = 2)

  keyIgnorePause(KEY_1, onKeyDown = ship.flightMode = 1)
  keyIgnorePause(KEY_2, onKeyDown = ship.flightMode = 2)
  keyIgnorePause(KEY_3, onKeyDown = ship.flightMode = 3)
  keyIgnorePause(KEY_4, onKeyDown = ship.flightMode = 4)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {pause(); HelpScreen.run()})

  keyIgnorePause(KEY_N, 100, onKeyDown = continueFutureTrajectory())
  keyIgnorePause(KEY_C, onKeyDown = updateFutureTrajectory())

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01f) {
      if(globalScale > 1) globalScale -= 1
      else if(globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
      if(globalScale < 0.01f) globalScale = 0.01f
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

  actionIgnorePause {
    if(future_trajectory.isEmpty) updateFutureTrajectory()
  }

  action {
    future_trajectory --= future_trajectory.takeWhile(_._1 < _time)
    future_trajectory_map.values.foreach(t => t --= t.takeWhile(_._1 < _time))
    nextStep()
  }

  private var _center = ship.coord
  center = _center

  render {    // TODO: display list!
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

    future_trajectory_map.foreach {
      case (index, body_states) => drawSlidingLines(body_states.map(_._2.coord), color = YELLOW)
    }
  }

  interface {
    print(fps, 20, windowHeight - 20, align = "top-left", color = DARK_GRAY)
    if(onPause) print("Пауза", windowCenter, align = "center", color = WHITE)
    print("F1 - Справка, F2, F3, F4 - режимы камеры, P - пауза", windowWidth - 20, 20, align = "bottom-right", color = GREEN)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)

    print(s"Режим камеры: $viewModeStr",
      20, 300, ORANGE)
    print(s"Ускорение времени: x${_time_mulitplier}",
      20, 280, ORANGE)
    print(s"Время: ${timeStr(_time/60)}",
      20, 260, ORANGE)
    print(s"Полетный режим: ${ship.flightModeStr}",
      20, 240, ORANGE)
    print(s"Время работы двигателей: ${ship.engines_worktime_tacts} тактов (${timeStr(ship.engines_worktime_tacts*timeMultiplier/60)})",
      20, 220, ORANGE)
    print(f"Мощность двигателей: ${ship.engines.find(_.active).map(e => e.power/e.max_power*100f).getOrElse(0f)}%.2f%",
      20, 200, ORANGE)
    print(f"Расстояние от центра звезды: ${ship.coord.dist(sun.coord)}%.2f м",
      20, 180, ORANGE)
    print(f"Скорость относительно центра звезды: ${ship.linearVelocity.*((ship.coord - sun.coord).n)*60*base_dt}%.2f м/сек",
      20, 160, ORANGE)
    print(f"Позиция: ${ship.coord.x}%.2f : ${ship.coord.y}%.2f",
      20, 140, ORANGE)
    print(f"Угловая скорость: ${ship.angularVelocity*60*base_dt}%.2f град/сек",
      20, 120, ORANGE)
    print(f"Скорость: ${ship.linearVelocity.norma*60*base_dt}%.2f м/сек ( velx = ${ship.linearVelocity.x*60*base_dt}%.2f м/сек, vely = ${ship.linearVelocity.y*60*base_dt}%.2f м/сек)",
      20, 100, ORANGE)
    print(f"Скорость для спутника в данной точке: ${satelliteSpeedInPoint(ship.coord, ship.linearVelocity)}",
      20, 80, ORANGE)
    print(f"Линейная скорость в момент отключения двигателей: ${linearSpeedWhenEnginesOff}",
      20, 60, ORANGE)
    print(f"Угловая скорость в момент отключения двигателей: ${angularSpeedWhenEnginesOff}",
      20, 40, ORANGE)
    print(f"Угол: ${ship.rotation}%.2f град",
      20, 20, ORANGE)
  }

  pause()
}

import OrbitalKiller._

class Planet(val index:String, val mass:Float, val init_coord:Vec, val init_velocity:Vec, val radius:Float) {
  private var skipped_points = 0
  private val body_trajectory = ArrayBuffer[Vec]()

  action {
    if(skipped_points == trajectory_accuracy-1) {
      body_trajectory += coord
      skipped_points = 0
    } else skipped_points += 1

    if(body_trajectory.size >= trajectory_capacity) body_trajectory.remove(0, 1000)
  }

  render {
    drawCircle(coord, radius, color = WHITE)
    drawCircle(coord, 3*radius, color = DARK_GRAY)
    drawSlidingLines(body_trajectory, color = GREEN)
  }

  def coord = currentState.coord

  def currentState = currentBodyState(index).getOrElse(BodyState(index, mass, 0f, Vec.zero, Vec.zero, init_velocity, init_coord, 0f, 0f, 0f, 0f))
}

class Star(val mass:Float, val coord:Vec, val radius:Float) {
  render {
    drawCircle(coord, radius, color = WHITE)
    drawCircle(coord, 3*radius, color = DARK_GRAY)
  }
}

case class Engine(position:Vec, force_dir:Vec, max_power:Float, sin_angle:Float, ship:Ship) {
  private var _power:Float = 1f
  def power = _power
  def power_=(new_power:Float) {
    if(new_power >= 1f && new_power < max_power && new_power != _power) {
      _power = new_power
      updateFutureTrajectory()
    }
  }

  def force = force_dir*power
  def torque = force.norma*position.norma*sin_angle
  private var is_active:Boolean = false
  def active = is_active
  def active_=(bool:Boolean) {
    if(is_active != bool) {
      is_active = bool
      if(is_active) {
        _power = 1f
        if(ship.engines_worktime_tacts == 0) {
          ship.engines_worktime_tacts = 10
          ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*timeMultiplier
        }
      }
      updateFutureTrajectory()
    }
  }
  def switchActive() {
    is_active = !is_active
    if(is_active) {
      _power = 1f
      ship.engines_worktime_tacts = 10
      ship.engines_stop_moment_seconds = time + ship.engines_worktime_tacts*timeMultiplier
    }
    updateFutureTrajectory()
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
  val index = s"${System.currentTimeMillis()}-${(math.random*1000).toInt}"

  var engines_worktime_tacts = 0l
  var engines_stop_moment_seconds = 0l

  val mass:Float = 1   // mass
  val moment_of_inertia = mass*(a*a + b*b)/12

  def currentState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      moment_of_inertia,
      Vec.zero,
      Vec.zero,
      init_velocity,
      init_coord,
      0f,
      0f,
      0f,
      init_rotation)
  )

  def force = currentState.force

  def linearAcceleration = currentState.acc

  def linearVelocity = currentState.vel

  def coord = currentState.coord

  def torque = currentState.torque

  def angularAcceleration = currentState.ang_acc

  def angularVelocity = currentState.ang_vel

  def rotation = currentState.ang

  private var flight_mode = 1 // 1 - free, 2 - stop rotation, 3 - axis orientation, 4 - orbit orientation
  def flightMode = flight_mode
  def flightMode_=(new_flight_mode:Int) {
    if(new_flight_mode > 0 && new_flight_mode < 5) {
      flight_mode = new_flight_mode
    }
  }
  def flightModeStr = flight_mode match {
    case 1 => "свободный"
    case 2 => "запрет вращения"
    case 3 => "ориентация по осям"
    case 4 => "ориентация по траектории"
    case _ =>
  }

  val two   = Engine(position = Vec(0, -b/2), force_dir = Vec(0, 1),  max_power = 10, sin_angle = 0, this)
  val eight = Engine(position = Vec(0, b/2),  force_dir = Vec(0, -1), max_power = 10, sin_angle = 0, this)

  val seven = Engine(position = Vec(-a/2, b/4), force_dir = Vec(1, 0),  max_power = 10, sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat,  this)
  val nine  = Engine(position = Vec(a/2, b/4),  force_dir = Vec(-1, 0), max_power = 10, sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)

  val four  = Engine(position = Vec(-a/2, 0), force_dir = Vec(1, 0),  max_power = 10, sin_angle = 0, this)
  val six   = Engine(position = Vec(a/2, 0),  force_dir = Vec(-1, 0), max_power = 10, sin_angle = 0, this)

  val one   = Engine(position = Vec(-a/2, -b/4), force_dir = Vec(1, 0),  max_power = 10, sin_angle = -(b/4)/math.sqrt(a*a/4 + b*b/16).toFloat, this)
  val three = Engine(position = Vec(a/2, -b/4),  force_dir = Vec(-1, 0), max_power = 10, sin_angle = (b/4)/math.sqrt(a*a/4 + b*b/16).toFloat,  this)

  val engines = List(one, two, three, four, six, seven, eight, nine)

  def currentReactiveForce(time:Long, bs:BodyState):Vec = {
    if(time < engines_stop_moment_seconds) {
      engines.filter(_.active).foldLeft(Vec.zero) {
        case (sum, e) => sum + e.force.rotateRad(bs.ang)
      }
    } else Vec.zero
  }

  def currentTorque(time:Long, bs:BodyState):Float = {
    if(time < engines_stop_moment_seconds) {
      engines.filter(_.active).foldLeft(0f) {
        case (sum, e) => sum + e.torque
      }
    } else 0f
  }

  private var skipped_points = 0
  private val body_trajectory = ArrayBuffer[Vec]()


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

  private def rotateRight() {
    seven.active = true
    engines.withFilter(e => e.active && e != seven).foreach(_.active = false)
  }

  private def rotateLeft() {
    nine.active = true
    engines.withFilter(e => e.active && e != nine).foreach(_.active = false)
  }

  private def preserveAngularVelocity(ang_vel_deg:Float) {
    val difference = angularVelocity - ang_vel_deg
    if(difference > 0.01f) rotateRight()
    else if(difference < -0.01f) rotateLeft()
  }

  private def preserveAngle(angle_deg:Float) {
    if(rotation != angle_deg) {
      if(rotation > angle_deg) {
        if(rotation - angle_deg > 20) preserveAngularVelocity(-10)
        if(rotation - angle_deg > 10) preserveAngularVelocity(-5)
        else if(rotation - angle_deg > 1) preserveAngularVelocity(-1)
        else if(rotation - angle_deg > 0.1f) preserveAngularVelocity(-0.1f)
        else preserveAngularVelocity(0)
      } else if(rotation < angle_deg) {
        if(rotation - angle_deg < -20) preserveAngularVelocity(10)
        if(rotation - angle_deg < -10) preserveAngularVelocity(5)
        else if(rotation - angle_deg < -1) preserveAngularVelocity(1)
        else if(rotation - angle_deg < -0.1f) preserveAngularVelocity(0.1f)
        else preserveAngularVelocity(0)
      }
    }
  }



  action {
    if(engines_worktime_tacts > 0) engines_worktime_tacts -= 1
    else if(engines.exists(_.active)) engines.foreach(_.active = false)

    if(skipped_points == trajectory_accuracy-1) {
      body_trajectory += coord
      skipped_points = 0
    } else skipped_points += 1

    if(body_trajectory.size >= trajectory_capacity) body_trajectory.remove(0, 1000)

    flight_mode match {
      case 1 =>
      case 2 => // запрет вращения
        if(math.abs(angularVelocity) < 0.01f) flight_mode = 1
        else preserveAngularVelocity(0)
      case 3 => // ориентация по осям
        if(math.abs(rotation) < 0.1f) flight_mode = 2
        else preserveAngle(0)
      case 4 => // ориентация по траектории
        val r = Vec(0,1).deg(linearVelocity)
        val angle = if(linearVelocity.x > 0) {
          val x = 360-r
          val y = -r
          if(math.abs(rotation - x) < math.abs(rotation - y)) x else y
        } else r
        if(math.abs(angle - rotation) < 0.1f) flight_mode = 2
        else preserveAngle(angle)
      case _ =>
    }
  }

  render {
    openglLocalTransform {
      openglMove(coord)
      openglRotateDeg(rotation)
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
    drawLine(coord, coord + force.n*20, RED)           // current force
    drawLine(coord, coord + linearVelocity.n*20, CYAN) // current velocity

    drawSlidingLines(body_trajectory, color = GREEN)
  }
}
