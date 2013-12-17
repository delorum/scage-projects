package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import collection.mutable.ArrayBuffer
import scala.collection.mutable

// TODO: implement this
sealed trait ViewMode
sealed trait FlightMode

object OrbitalKiller extends ScageScreenApp("Orbital Killer", 1024, 768) {

  private var _time_mulitplier = 1
  def timeMultiplier = _time_mulitplier
  def timeMultiplier_=(tm:Int) {if(tm > 0) _time_mulitplier = tm}
  private var _dt:Float = base_dt
  def dt = _dt

  private var _time = 0l   // tacts
  def time = _time

  def timeStr(time_sec:Long):String = {
    val sec  = 1l
    val min  = sec*60
    val hour  = min*60
    val day  = hour*24
    s"${time_sec/day} дн ${time_sec%day/hour} ч ${time_sec%hour/min} мин ${time_sec%min/sec} сек"
  }

  private var continue_future_trajectory = false

  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(time:Long, body_states:List[BodyState]) = systemEvolutionFrom(
    dt, elasticity = 0.9f,
    force = (time, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          gravityForce(sun.coord, sun.mass, bs.coord, bs.mass, G) +
          other_bodies.find(_.index == earth.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(Vec.zero) +
          ship.currentReactiveForce(time, bs)
        case earth.index =>
          gravityForce(sun.coord, sun.mass, bs.coord, bs.mass, G)
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

  private val body_trajectories_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()

  private def updateFutureTrajectoryMap() {
    future_trajectory_map.clear()
    future_trajectory
      .zipWithIndex
      .withFilter(_._2 % trajectory_accuracy == 0)
      .map(_._1)
      .foreach {
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
    }
    updateFutureTrajectoryMap()
  }

  private def continueFutureTrajectoryMap(steps:Seq[(Long, List[BodyState])]) {
    steps
      .zipWithIndex
      .withFilter(_._2 % trajectory_accuracy == 0)
      .map(_._1)
      .foreach {
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
    }.toSeq
    future_trajectory ++= steps
    continueFutureTrajectoryMap(steps)
  }

  val sun = new Star("sun", mass = 10000, coord = Vec.zero, radius = 3000)
  val earth_start_position = Vec(-sun.radius*20f, sun.radius*20f)
  val earth_init_velocity = satelliteSpeed(earth_start_position, sun.coord, sun.mass, G)
  val earth = new Planet(
    "earth",
    mass = 1000,
    init_coord = earth_start_position,
    init_velocity = earth_init_velocity,
    radius = 1000)

  /*val ship_start_position = sun.coord + Vec(sun.radius*1.5f, sun.radius*1.5f)*/
  /*val ship_init_velocity = satelliteSpeed(ship_start_position, sun.coord, sun.mass)*/
  val ship_start_position = earth.coord + Vec(earth.radius*1.5f, earth.radius*1.5f)
  val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.mass, G)
  val ship = new Ship2("ship",
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 45
  )

  private val real_system_evolution =
    futureSystemEvolutionFrom(0, List(ship.currentState, earth.currentState, sun.currentState)).iterator

  private var skipped_points = 0
  private def nextStep() {
    val (t, body_states) = real_system_evolution.next()
    _time = t
    if(skipped_points == trajectory_accuracy-1) {
      body_states.foreach(bs => {
        current_body_states(bs.index) = bs
        val body_trajectory = body_trajectories_map.getOrElseUpdate(bs.index, ArrayBuffer[(Long, BodyState)]())
        body_trajectory += (_time -> bs)
        if(body_trajectory.size >= trajectory_capacity) body_trajectory.remove(0, 1000)
      })
      skipped_points = 0
    } else {
      body_states.foreach(bs => {
        current_body_states(bs.index) = bs
      })
      skipped_points += 1
    }
  }
  nextStep()

  private var view_mode = 0

  private def freeCenter() {
    _center = center
    center = _center
  }

  def viewMode = view_mode
  def viewMode_=(new_view_mode:Int) {
    new_view_mode match {
      case 0 => // свободный
        _center = center
        center = _center
        rotationAngle = 0
        view_mode = 0
      case 1 => // фиксация на корабле
        center = ship.coord
        rotationPoint = ship.coord
        rotationAngleDeg = -ship.rotation
        view_mode = 1
      case 2 => // посадка на планету
        center = ship.coord
        rotationPoint = ship.coord
        rotationAngleDeg = {
          val nearest_body_coord = if(ship.coord.dist2(sun.coord) < ship.coord.dist2(earth.coord)) sun.coord else earth.coord
          val vec = ship.coord - nearest_body_coord
          if(vec.x >= 0) vec.deg(Vec(0, 1))
          else vec.deg(Vec(0, 1)) *(-1)
        }
        view_mode = 2
      case 3 => // фиксация на солнце
        center = sun.coord
        rotationAngle = 0
        view_mode = 3
      case 4 => // фиксация на планете
        center = earth.coord
        rotationAngle = 0
        view_mode = 4
      case _ =>
    }
  }
  private def viewModeStr = view_mode match {
    case 0 => "свободный"
    case 1 => "фиксация на корабле"
    case 2 => "посадка на планету"
    case 3 => "фиксация на солнце"
    case 4 => "фиксация на планете"
    case _ => ""
  }

  def satelliteSpeedInPoint(coord:Vec):String = {
    if(coord.dist(earth.coord) < earth.gravitational_radius) {
      val ss = earth.linearVelocity + satelliteSpeed(coord, earth.coord, earth.mass, G)
      f"${ss.norma*60*base_dt}%.2f м/сек (velx = ${ss.x*60*base_dt}%.2f м/сек, vely = ${ss.y*60*base_dt}%.2f м/сек)"
    } else if(coord.dist(sun.coord) < sun.gravitational_radius) {
      val ss = satelliteSpeed(coord, sun.coord, sun.mass, G)
      f"${ss.norma*60*base_dt}%.2f м/сек (velx = ${ss.x*60*base_dt}%.2f м/сек, vely = ${ss.y*60*base_dt}%.2f м/сек)"
    } else "N/A"
  }

  def linearSpeedWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentSeconds).max) match {
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
  }

  def angularSpeedWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentSeconds).max) match {
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
  }

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD1)})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD2)})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD3)})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD4)})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD6)})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD7)})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD8)})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD9)})

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {ship.engines.foreach(_.active = false)})

  keyIgnorePause(KEY_UP,   10, onKeyDown = {ship.selected_engine.foreach(e => e.power += 0.1f)})
  keyIgnorePause(KEY_DOWN, 10, onKeyDown = {ship.selected_engine.foreach(e => e.power -= 0.1f)})
  keyIgnorePause(KEY_RIGHT,   10, onKeyDown = {
    ship.selected_engine.foreach(e => {
      e.worktimeTacts += 1
      updateFutureTrajectory()
    })
  })
  keyIgnorePause(KEY_LEFT, 10, onKeyDown = {
    ship.selected_engine.foreach(e => {
      if(e.worktimeTacts > 0) {
        e.worktimeTacts -= 1
        updateFutureTrajectory()
      }
    })
  })

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    _time_mulitplier += 1
    _dt = _time_mulitplier*base_dt
    ship.selected_engine.foreach(e => {
      e.worktimeTacts = e.worktimeTacts
    })
  })
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    if(_time_mulitplier > 1) {
      _time_mulitplier -= 1
      _dt = _time_mulitplier*base_dt
      ship.engines.filter(_.active).foreach(e => {
        e.worktimeTacts = e.worktimeTacts
      })
    }
  })

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    _time_mulitplier += 40
    _dt = _time_mulitplier*base_dt
    ship.engines.filter(_.active).foreach(e => {
      e.worktimeTacts = e.worktimeTacts
    })
  })
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    if (_time_mulitplier != 1) {
      _time_mulitplier = 1
      _dt = _time_mulitplier*base_dt
      ship.engines.filter(_.active).foreach(e => {
        e.worktimeTacts = e.worktimeTacts
      })
    }
  })

  keyIgnorePause(KEY_W, 10, onKeyDown = {freeCenter(); _center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {freeCenter(); _center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {freeCenter(); _center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {freeCenter(); _center += Vec(5/globalScale, 0)})

  keyIgnorePause(KEY_SPACE, onKeyDown = _center = ship.coord)

  keyIgnorePause(KEY_1, onKeyDown = ship.flightMode = 1)
  keyIgnorePause(KEY_2, onKeyDown = ship.flightMode = 2)
  keyIgnorePause(KEY_3, onKeyDown = ship.flightMode = 3)
  keyIgnorePause(KEY_4, onKeyDown = ship.flightMode = 4)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {pause(); HelpScreen.run()})
  keyIgnorePause(KEY_F2, onKeyDown = {viewMode = 0; rotationAngle = 0})  // свободный
  keyIgnorePause(KEY_F3, onKeyDown = viewMode = 1)  // фиксация на корабле
  keyIgnorePause(KEY_F4, onKeyDown = viewMode = 2)  // посадка на планету
  keyIgnorePause(KEY_F5, onKeyDown = viewMode = 3)  // фиксация на солнце
  keyIgnorePause(KEY_F6, onKeyDown = viewMode = 4)  // фиксация на планете

  keyIgnorePause(KEY_N, onKeyDown = continue_future_trajectory = !continue_future_trajectory)
  keyIgnorePause(KEY_C, onKeyDown = {
    continue_future_trajectory = false
    updateFutureTrajectory()
  })

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01f) {
      if(globalScale > 1) globalScale -= 1
      else if(globalScale > 0.1f) globalScale -= 0.1f
      else globalScale -= 0.01f
      if(globalScale < 0.01f) globalScale = 0.01f
    }
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < 5) {
      if(globalScale < 1) globalScale += 0.1f
      else globalScale += 1
    }
  })

  actionIgnorePause {
    if(future_trajectory.isEmpty) updateFutureTrajectory()
  }

  actionIgnorePause(100) {
    if(continue_future_trajectory) continueFutureTrajectory()
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
      case (index, body_states) =>
        index match {
          case "ship" =>
            viewMode match {
              case 4 =>
                drawSlidingLines(
                  for {
                    ((_, bs), (_, earth_bs)) <- body_states.zip(future_trajectory_map(earth.index))
                    coord = bs.coord - earth_bs.coord + earth.coord
                  } yield coord,
                  color = YELLOW)
              case _ =>
                drawSlidingLines(body_states.map(_._2.coord), color = YELLOW)
            }
          case _ =>
            drawSlidingLines(body_states.map(_._2.coord), color = YELLOW)
        }
    }

    body_trajectories_map.foreach {
      case (index, body_states) =>
        index match {
          case "ship" =>
            viewMode match {
              case 4 =>
                drawSlidingLines(
                  for {
                    ((_, bs), (_, earth_bs)) <- body_states.zip(body_trajectories_map(earth.index))
                    coord = bs.coord - earth_bs.coord + earth.coord
                  } yield coord,
                  color = GREEN)
              case _ =>
                drawSlidingLines(body_states.map(_._2.coord), color = GREEN)
            }
          case _ =>
            drawSlidingLines(body_states.map(_._2.coord), color = GREEN)
        }
    }
  }

  interface {
    print(fps, 20, windowHeight - 20, align = "top-left", color = DARK_GRAY)
    if(onPause) print("Пауза", windowCenter, align = "center", color = WHITE)
    print("F1 - Справка, P - пауза", windowWidth - 20, 20, align = "bottom-right", color = GREEN)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)

    print(s"Ускорение времени: x${_time_mulitplier}",
      20, 320, ORANGE)
    print(s"Время: ${timeStr(_time/60)}",
      20, 300, ORANGE)
    print(s"Режим камеры: $viewModeStr",
      20, 280, ORANGE)
    print(s"Полетный режим: ${ship.flightModeStr}",
      20, 260, ORANGE)
    print(f"Расстояние и скорость относительно звезды: ${ship.coord.dist(sun.coord)}%.2f м, ${ship.linearVelocity* (ship.coord - sun.coord).n *60*base_dt}%.2f м/сек",
      20, 240, ORANGE)
    print(f"Расстояние и скорость относительно планеты: ${ship.coord.dist(earth.coord)}%.2f м, ${ship.linearVelocity* (ship.coord - earth.coord).n *60*base_dt}%.2f м/сек",
      20, 220, ORANGE)
    print(f"Скорость: ${ship.linearVelocity.norma*60*base_dt}%.2f м/сек ( velx = ${ship.linearVelocity.x*60*base_dt}%.2f м/сек, vely = ${ship.linearVelocity.y*60*base_dt}%.2f м/сек)",
      20, 200, ORANGE)
    print(f"Позиция: ${ship.coord.x}%.2f : ${ship.coord.y}%.2f",
      20, 180, ORANGE)
    print(f"Угловая скорость: ${ship.angularVelocity*60*base_dt}%.2f град/сек",
      20, 160, ORANGE)
    print(f"Угол: ${ship.rotation}%.2f град",
      20, 140, ORANGE)
    print(f"Скорость для спутника в данной точке: ${satelliteSpeedInPoint(ship.coord)}",
      20, 120, ORANGE)
    print(f"Линейная скорость в момент отключения двигателей: $linearSpeedWhenEnginesOff",
      20, 100, ORANGE)
    print(f"Угловая скорость в момент отключения двигателей: $angularSpeedWhenEnginesOff",
      20, 80, ORANGE)
    print(s"Двигательная установка: ${if(ship.engines.exists(_.active)) "[rактивирована]" else "отключена"}",
      20, 60, ORANGE)
    val engines_work_tacts = maxOption(ship.engines.withFilter(_.active).map(_.worktimeTacts)).getOrElse(0l)
    print(s"Время работы двигателей: $engines_work_tacts тактов (${timeStr(maxOption(ship.engines.withFilter(_.active).map(_.worktimeTacts*_time_mulitplier/60)).getOrElse(0l))})",
      20, 40, ORANGE)
    print(s"Расчет траектории: ${if(continue_future_trajectory) "[rактивирован]" else "отключен"}",
      20, 20, ORANGE)
  }

  pause()
}

import OrbitalKiller._

class Planet(val index:String, val mass:Float, val init_coord:Vec, val init_velocity:Vec, val radius:Float) {
  lazy val gravitational_radius = {
    (coord.dist(sun.coord)*math.sqrt(mass/20f/sun.mass)/(1 + math.sqrt(mass/20f/sun.mass))).toFloat
  }

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = CircleShape(radius),
      is_static = false))

  render {
    drawCircle(coord, radius, color = WHITE)
    drawCircle(coord, gravitational_radius, color = DARK_GRAY)
    print(index, coord, size = max_font_size/globalScale*2f, WHITE, align = "center")
  }
}

class Star(val index:String, val mass:Float, val coord:Vec, val radius:Float) {
  lazy val gravitational_radius = {
    (coord.dist(earth.coord)*math.sqrt(mass/20f/earth.mass)/(1 + math.sqrt(mass/20f/earth.mass))).toFloat
  }

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = Vec.zero,
      vel = Vec.zero,
      coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = 0f,
      shape = CircleShape(radius),
      is_static = true))

  render {
    drawCircle(coord, radius, color = WHITE)
    drawCircle(coord, gravitational_radius, color = DARK_GRAY)
    print(index, coord, size = max_font_size/globalScale*2f, WHITE, align = "center")
  }
}






