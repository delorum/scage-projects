package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._

import scala.collection.mutable.ArrayBuffer

//import collection.mutable.ArrayBuffer
import scala.collection.mutable

// TODO: implement this
sealed trait ViewMode
sealed trait FlightMode

object OrbitalKiller extends ScageScreenAppD("Orbital Killer", 1280, 768) {
  val k:Double = 1 // доля секунды симуляции, которая обрабатывается за один тик, если не применяется ускорение

  // движок делает вызов обработчика примерно 60 раз в секунду, за каждый вызов будет обрабатывать вот такую порцию симуляции
  // то есть, мы хотим, чтобы за одну реальную секунду обрабатывалось k секунд симуляции, поэтому за один такт движка (которых 60 в секунду)
  // будем обрабатывать k/60
  val base_dt:Double = 1.0/63*k

  val realtime = (1.0/k).toInt // 1/k*baseDt соответствует реальному течению времени

  private var _time_multiplier = realtime
  def timeMultiplier = {
    if(_time_multiplier != realtime && ships.flatMap(_.engines).exists(_.active)) {
      timeMultiplier_=(realtime)
    }
    _time_multiplier
  }
  def timeMultiplier_=(new_time_multiplier:Int) {
    if(new_time_multiplier > 0) {
      // разрешаем переход на ускоренное/замедленное течение времени только если все двигатели выключены
      if(new_time_multiplier == realtime || ships.flatMap(_.engines).forall(!_.active)) {
        _time_multiplier = new_time_multiplier
        ships.flatMap(_.engines).filter(_.active).foreach(e => {
          e.worktimeTacts = e.worktimeTacts
        })

      }
    }
  }

  def dt = timeMultiplier*base_dt

  private var _tacts = 0l
  def tacts:Long = _tacts

  def timeStr(time_msec:Long):String = {
    val msec  = 1l
    val sec  = msec*1000
    val min  = sec*60
    val hour  = min*60
    val day  = hour*24
    s"${time_msec/day} дн ${time_msec%day/hour} ч ${time_msec%hour/min} мин ${time_msec%min/sec} сек ${time_msec%sec/msec} мсек"
  }

  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentBodyStates = current_body_states.values.toList

  def futureSystemEvolutionFrom(dt: => Double, tacts:Long, body_states:List[BodyState], enable_collisions:Boolean) = systemEvolutionFrom(
    dt, base_dt, elasticity = 0.9,
    force = (tacts, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
          other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero) +
          ship.currentReactiveForce(tacts, bs)
        case station.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
            other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero) +
            station.currentReactiveForce(tacts, bs)
        case moon.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G)
        case _ => DVec.dzero
      }
    },
    torque = (tacts, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          ship.currentTorque(tacts, bs)
        case _ =>
          0
      }
    },
    enable_collisions = enable_collisions)((tacts, body_states))

  //private var continue_future_trajectory = false
  //val trajectory_accuracy = 100 // для рисования: рисуем только каждую сотую точку траектории
  //val trajectory_capacity = 100000

  private val future_trajectory = ArrayBuffer[(Long, List[BodyState])]()
  //private val future_trajectory_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()
  private val future_trajectory_capacity = 10000

  //private val body_trajectories_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()

  def updateFutureTrajectory() {
    future_trajectory.clear()
    future_trajectory ++= {
      futureSystemEvolutionFrom(dt, _tacts, currentBodyStates, enable_collisions = false)
        .take(future_trajectory_capacity)
    }
    //updateFutureTrajectoryMap()
  }

  /*private def updateFutureTrajectoryMap() {
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
  }*/

  def continueFutureTrajectory() {
    val (t, s) = future_trajectory.lastOption.getOrElse((_tacts, currentBodyStates))
    val steps = {
      futureSystemEvolutionFrom(dt, t, s, enable_collisions = false)
        .take(future_trajectory_capacity)
    }.toSeq
    future_trajectory ++= steps
    //continueFutureTrajectoryMap(steps)
  }

  /*private def continueFutureTrajectoryMap(steps:Seq[(Long, List[BodyState])]) {
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
  }*/

  val earth = new Star("Earth", mass = 5.9746E24, coord = DVec.dzero, radius = 6400000)

  val moon_start_position = DVec(-269000000, 269000000)
  val moon_init_velocity = satelliteSpeed(moon_start_position, earth.coord, earth.linearVelocity, earth.mass, G)
  val moon = new Planet(
    "Moon",
    mass = 7.3477E22,
    init_coord = moon_start_position,
    init_velocity = moon_init_velocity,
    radius = 1737000)

  //val ship_start_position = earth.coord + DVec(0, earth.radius + 100000)
  //val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G)
  val ship_start_position = moon.coord + DVec(0, moon.radius + 500)
  val ship_init_velocity = satelliteSpeed(ship_start_position, moon.coord, moon.linearVelocity, moon.mass, G)
  val ship = new Ship3("ship",
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 45
  )

  val station_start_position = earth.coord + DVec(300, earth.radius + 100000)
  val station_init_velocity = satelliteSpeed(station_start_position, earth.coord, earth.linearVelocity, earth.mass, G)
  val station = new SpaceStation("station",
    init_coord = station_start_position,
    init_velocity = station_init_velocity,
    init_rotation = 45
  )
  
  val ships = List(ship, station)

  private val real_system_evolution =
    futureSystemEvolutionFrom(dt, 0, List(
        ship.currentState,
        station.currentState,
        moon.currentState,
        earth.currentState),
      enable_collisions = true).iterator

  //private var skipped_points = 0
  private def nextStep() {
    val (t, body_states) = real_system_evolution.next()
    _tacts = t
    /*if(skipped_points == trajectory_accuracy-1) {
      body_states.foreach(bs => {
        current_body_states(bs.index) = bs
        /*val body_trajectory = body_trajectories_map.getOrElseUpdate(bs.index, ArrayBuffer[(Long, BodyState)]())
        body_trajectory += (_tacts -> bs)
        if(body_trajectory.size >= trajectory_capacity) body_trajectory.remove(0, 1000)*/
      })
      skipped_points = 0
    } else {*/
      body_states.foreach(bs => {
        current_body_states(bs.index) = bs
      })
      //skipped_points += 1
    /*}*/
  }
  nextStep()

  private var view_mode = 0

  /*private def freeCenter() {
    _center = center
    center = _center
  }*/

  def viewMode = view_mode
  def viewMode_=(new_view_mode:Int) {
    new_view_mode match {
      case 0 => // свободный
        _center = center
        center = _center
        rotationAngle = 0
        base = DVec.zero
        view_mode = 0
      case 1 => // фиксация на корабле
        center = ship.coord + _ship_offset
        base = if(ship.coord.norma < 100000) DVec.zero else ship.coord
        rotationCenter = ship.coord
        rotationAngleDeg = -ship.rotation
        view_mode = 1
      case 2 => // посадка на планету
        center = ship.coord
        rotationCenter = ship.coord
        rotationAngleDeg = {
          val nearest_body_coord = if(ship.coord.dist2(earth.coord) < ship.coord.dist2(moon.coord)) earth.coord else moon.coord
          val vec = ship.coord - nearest_body_coord
          if(vec.x >= 0) vec.deg(DVec(0, 1))
          else vec.deg(DVec(0, 1)) * (-1)
        }
        view_mode = 2
      case 3 => // фиксация на солнце
        center = earth.coord
        rotationAngle = 0
        view_mode = 3
      case 4 => // фиксация на планете
        center = moon.coord
        rotationAngle = 0
        view_mode = 4
      case _ =>
    }
  }
  private def viewModeStr = view_mode match {
    case 0 => "свободный"
    case 1 => "фиксация на корабле"
    case 2 => "посадка"
    case 3 => "фиксация на Земле"
    case 4 => "фиксация на Луне"
    case _ => ""
  }

  def mOrKm(meters:Number):String = {
    if(math.abs(meters.floatValue()) < 1000) f"${meters.floatValue()}%.2f м" else f"${meters.floatValue()/1000}%.2f км"
  }

  def msecOrKmsec(meters:Number):String = {
    if(math.abs(meters.floatValue()) < 1000) f"${meters.floatValue()}%.2f м/сек" else f"${meters.floatValue()/1000}%.2f км/сек"
  }

  def satelliteSpeedStrInPoint(coord:DVec):String = {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(planet) =>
        val ss = satelliteSpeed(coord, planet.coord, planet.linearVelocity, planet.mass, G)
        f"${msecOrKmsec(ss.norma)} (velx = ${msecOrKmsec(ss.x)}, vely = ${msecOrKmsec(ss.y)})"
      case None =>
        "N/A"
    }
  }

  def escapeVelocityStrInPoint(coord:DVec):String = {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(planet) =>
        val ss = escapeVelocity(coord, planet.coord, planet.linearVelocity, planet.mass, G)
        f"${msecOrKmsec(ss.norma)} (velx = ${msecOrKmsec(ss.x)}, vely = ${msecOrKmsec(ss.y)})"
      case None =>
        "N/A"
    }
  }

  def orbitStrInPointWithVelocity(mass:Double, coord:DVec, velocity:DVec):String = {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(planet) =>
        val Orbit(a, b, e, c, p, r_p, r_a, t, f1, f2, center) = calculateOrbit(planet.mass, planet.coord, mass, coord - planet.coord, velocity - planet.linearVelocity, G)
        f"e = $e%.2f, r_p = ${mOrKm(r_p - planet.radius)}, r_a = ${mOrKm(r_a - planet.radius)}, t = ${timeStr((t*1000l).toLong)}"
      case None => "N/A"
    }
  }

  def orbitInPointWithVelocity(mass:Double, coord:DVec, velocity:DVec):Option[Orbit] = {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(planet) =>
        Some(calculateOrbit(planet.mass, planet.coord, mass, coord - planet.coord, velocity - planet.linearVelocity, G))
      case None => None
    }
  }

  def orbitAroundCelestialInPointWithVelocity(mass:Double, coord:DVec, velocity:DVec):Option[(CelestialBody, Orbit)] = {
    insideGravitationalRadiusOfCelestialBody(coord) match {
      case Some(planet) =>
        Some((planet, calculateOrbit(planet.mass, planet.coord, mass, coord - planet.coord, velocity - planet.linearVelocity, G)))
      case None => None
    }
  }

  def linearSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
        case Some((t, lbs)) =>
          lbs.find(_.index == ship.index) match {
            case Some(bs) =>
              val s = bs.vel
              f"${msecOrKmsec(s.norma)} (velx = ${msecOrKmsec(s.x)}, vely = ${msecOrKmsec(s.y)})"
            case None => "N/A"
          }
        case None =>
          continueFutureTrajectory()
          "N/A"
      }
    }
  }

  def angularSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
        case Some((t, lbs)) =>
          lbs.find(_.index == ship.index) match {
            case Some(bs) =>
              val s = bs.ang_vel
              f" $s%.2f град/сек"
            case None => "N/A"
          }
        case None =>
          continueFutureTrajectory()
          "N/A"
      }
    }
  }

  def orbitParametersStrWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
        case Some((t, lbs)) =>
          lbs.find(_.index == ship.index) match {
            case Some(bs) =>
              orbitStrInPointWithVelocity(bs.mass, bs.coord, bs.vel)
            case None =>"N/A"
          }
        case None =>
          continueFutureTrajectory()
          "N/A"
      }
    }
  }

  /*private var disable_trajectory_drawing = false
  private var disable_future_trajectory_drawing = false*/
  private var disable_interface_drawing = false

  private var _draw_map_mode = false
  def drawMapMode = _draw_map_mode
  def drawMapMode_=(new_mode:Boolean) {
    if(new_mode) {
      _draw_map_mode = true
      orbitAroundCelestialInPointWithVelocity(ship.mass, ship.coord, ship.linearVelocity) match {
        case Some((planet, orbit)) =>
          val b = BoxShape(2*orbit.a, 2*orbit.b)
          val aabb = b.aabb(orbit.center, Vec(-1,0).signedDeg(orbit.f2-orbit.f1))
          viewMode = 0
          globalScale = 750 / (aabb.height * scale)
          _center = orbit.center*scale
        case None =>
          viewMode = 0
          globalScale = 1
          _center = earth.coord*scale
      }
    } else {
      _draw_map_mode = false
      globalScale = 1
      viewMode = 1
    }
  }

  def insideGravitationalRadiusOfCelestialBody(coord:DVec):Option[CelestialBody] = {
    if(coord.dist(moon.coord) < moon.gravitational_radius) Some(moon)
    else if(coord.dist(earth.coord) < earth.gravitational_radius) Some(earth)
    else None
  }

  def drawArrow(from1:DVec, to1:DVec, color:ScageColor, scale:Double = globalScale) {
    val arrow11 = to1 + ((from1-to1).n*10/scale).rotateDeg(15)
    val arrow12 = to1 + ((from1-to1).n*10/scale).rotateDeg(-15)
    drawLine(from1, to1, color)
    drawLine(to1, arrow11, color)
    drawLine(to1, arrow12, color)
  }

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD1)})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD2)})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD3)})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD4)})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD6)})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD7)})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD8)})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {ship.switchEngineActive(KEY_NUMPAD9)})

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {ship.engines.foreach(e => {e.active = false; e.power = 0})})

  keyIgnorePause(KEY_UP,   10, onKeyDown = {ship.selected_engine.foreach(e => e.powerPercent += 1)}, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_DOWN, 10, onKeyDown = {ship.selected_engine.foreach(e => e.powerPercent -= 1)}, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_RIGHT,   10, onKeyDown = {
    ship.selected_engine.foreach(e => {
      e.worktimeTacts += 1
      //updateFutureTrajectory()
    })
  }, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_LEFT, 10, onKeyDown = {
    ship.selected_engine.foreach(e => {
      /*if(e.worktimeTacts > 0) {*/
        e.worktimeTacts -= 1
        //updateFutureTrajectory()
      /*}*/
    })
  }, onKeyUp = updateFutureTrajectory())

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    timeMultiplier += realtime
  }, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    if(timeMultiplier > realtime) {
      timeMultiplier -= realtime
    }
  }, onKeyUp = updateFutureTrajectory())

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    if(timeMultiplier == realtime) {
      timeMultiplier = realtime*100
    } else {
      timeMultiplier += realtime*100
    }
  }, onKeyUp = updateFutureTrajectory())
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    if (timeMultiplier != realtime) {
      timeMultiplier = realtime
    }
  }, onKeyUp = updateFutureTrajectory())

  /*keyIgnorePause(KEY_W, 10, onKeyDown = {freeCenter(); _center += Vec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {freeCenter(); _center += Vec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {freeCenter(); _center += Vec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {freeCenter(); _center += Vec(5/globalScale, 0)})*/

  keyIgnorePause(KEY_W, 10, onKeyDown = {if(drawMapMode) _center += DVec(0, 5/globalScale)  else _ship_offset += DVec(0, 5/globalScale)})
  keyIgnorePause(KEY_A, 10, onKeyDown = {if(drawMapMode) _center += DVec(-5/globalScale, 0) else _ship_offset += DVec(-5/globalScale, 0)})
  keyIgnorePause(KEY_S, 10, onKeyDown = {if(drawMapMode) _center += DVec(0, -5/globalScale) else _ship_offset += DVec(0, -5/globalScale)})
  keyIgnorePause(KEY_D, 10, onKeyDown = {if(drawMapMode) _center += DVec(5/globalScale, 0)  else _ship_offset += DVec(5/globalScale, 0)})

  keyIgnorePause(KEY_M, onKeyDown = {drawMapMode = !drawMapMode})

  keyIgnorePause(KEY_SPACE, onKeyDown = {
    if(!drawMapMode) {
      //freeCenter()
      //_center = ship.coord
      _ship_offset = DVec.zero
    } else {
      drawMapMode = true
    }
  })

  keyIgnorePause(KEY_1, onKeyDown = ship.flightMode = 1)
  keyIgnorePause(KEY_2, onKeyDown = ship.flightMode = 2)
  keyIgnorePause(KEY_3, onKeyDown = ship.flightMode = 3)
  keyIgnorePause(KEY_4, onKeyDown = ship.flightMode = 4)
  keyIgnorePause(KEY_5, onKeyDown = ship.flightMode = 5)
  keyIgnorePause(KEY_6, onKeyDown = ship.flightMode = 6)
  keyIgnorePause(KEY_7, onKeyDown = ship.flightMode = 7)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {pause(); HelpScreen.run()})
  //keyIgnorePause(KEY_F2, onKeyDown = {viewMode = 0; rotationAngle = 0})  // свободный
  //keyIgnorePause(KEY_F3, onKeyDown = viewMode = 1)                       // фиксация на корабле
  //keyIgnorePause(KEY_F4, onKeyDown = viewMode = 2)                       // посадка на планету
  //keyIgnorePause(KEY_F5, onKeyDown = viewMode = 3)                       // фиксация на солнце
  //keyIgnorePause(KEY_F6, onKeyDown = viewMode = 4)                       // фиксация на планете

  /*keyIgnorePause(KEY_Z, onKeyDown = disable_trajectory_drawing = !disable_trajectory_drawing)
  keyIgnorePause(KEY_X, onKeyDown = disable_future_trajectory_drawing = !disable_future_trajectory_drawing)*/
  keyIgnorePause(KEY_V, onKeyDown = disable_interface_drawing = !disable_interface_drawing)
  //keyIgnorePause(KEY_N, onKeyDown = continue_future_trajectory = !continue_future_trajectory)
  /*keyIgnorePause(KEY_C, onKeyDown = {
    continue_future_trajectory = false
    /*updateFutureTrajectory()*/
  })*/

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01) {
      if(globalScale.toInt > 100000) globalScale -= 100000
      else if(globalScale.toInt > 10000) globalScale -= 10000
      else if(globalScale.toInt > 1000) globalScale -= 1000
      else if(globalScale.toInt > 100) globalScale -= 100
      else if(globalScale.toInt > 10) globalScale -= 10
      else if(globalScale.toInt > 1) globalScale -= 1
      else if((globalScale*10).toInt > 1) globalScale -= 0.1
      else globalScale -= 0.01
      if(globalScale < 0.01) globalScale = 0.01
    }
    println(s"$globalScale : ${windowCenter} : ${((windowCenter - center * globalScale)/globalScale + center)*globalScale}")
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    if(globalScale < (if(!drawMapMode) 5 else 1000000)) {
      if(globalScale < 0.1) globalScale += 0.01
      else if(globalScale < 1) globalScale += 0.1
      else if(globalScale < 10) globalScale +=1
      else if(globalScale < 100) globalScale +=10
      else if(globalScale < 1000) globalScale +=100
      else if(globalScale < 10000) globalScale +=1000
      else if(globalScale < 100000) globalScale +=10000
      else globalScale += 100000
    }
    println(s"$globalScale : ${windowCenter} : ${((windowCenter - center * globalScale)/globalScale + center)*globalScale}")
  })


  private var left_up_corner:Option[DVec] = None
  private var right_down_corner:Option[DVec] = None
  leftMouseIgnorePause(onBtnDown = m => {if(drawMapMode) left_up_corner = Some(absCoord(m))}, onBtnUp = m => {
    for {
      x <- left_up_corner
      y <- right_down_corner
    } {
      val c = (y-x).n*(y.dist(x)/2f)+x
      val h = math.abs(y.y - x.y)
      globalScale = 750 / h
      _center = c
    }
    left_up_corner = None
    right_down_corner = None})
  leftMouseDragIgnorePause(onDrag = m => if(drawMapMode) right_down_corner = Some(absCoord(m)))

  actionIgnorePause {
    if(future_trajectory.isEmpty) updateFutureTrajectory()
  }

  /*actionIgnorePause(100) {
    if(continue_future_trajectory) continueFutureTrajectory()
  }*/

  action {
    future_trajectory --= future_trajectory.takeWhile(_._1 < _tacts)
    //future_trajectory_map.values.foreach(t => t --= t.takeWhile(_._1 < _tacts))
    //if(future_trajectory.isEmpty) updateFutureTrajectory()
    nextStep()
  }

  private var _center = ship.coord
  private var _ship_offset = DVec.zero
  center = _center
  windowCenter = DVec((windowWidth-1024)+1024/2, windowHeight/2)
  viewMode = 1

  private val scale = 1e-6

  render {
    if(drawMapMode) {
      drawCircle(earth.coord*scale, earth.radius * scale, WHITE)
      drawCircle(earth.coord*scale, earth.gravitational_radius * scale, color = DARK_GRAY)

      drawCircle(moon.coord*scale, moon.radius * scale, WHITE)
      drawCircle(moon.coord*scale, moon.gravitational_radius * scale, color = DARK_GRAY)
      val moon_orbit = calculateOrbit(earth.mass, earth.coord, moon.mass, moon.coord, moon.linearVelocity, G)
      openglLocalTransform {
        openglMove(moon_orbit.center*scale)
        openglRotateDeg(Vec(-1,0).signedDeg(moon_orbit.f2-moon_orbit.f1))
        drawEllipse(DVec.zero, moon_orbit.a * scale, moon_orbit.b * scale, GREEN)
      }

      drawFilledCircle(ship.coord*scale, earth.radius * scale / 2f / globalScale, WHITE)
      orbitAroundCelestialInPointWithVelocity(ship.mass, ship.coord, ship.linearVelocity) match {
        case Some((planet, orbit)) =>
          openglLocalTransform {
            openglMove(orbit.center*scale)
            openglRotateDeg(Vec(-1,0).signedDeg(orbit.f2-orbit.f1))
            drawEllipse(DVec.zero, orbit.a * scale, orbit.b * scale, YELLOW)
          }
        case None =>
      }
      if(ship.engines.exists(_.active)) {
        future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
          case Some((t, lbs)) =>
            lbs.find(_.index == ship.index) match {
              case Some(bs) =>
                orbitAroundCelestialInPointWithVelocity(bs.mass, bs.coord, bs.vel) match {
                  case Some((planet, orbit)) =>
                    openglLocalTransform {
                      openglMove(orbit.center*scale)
                      openglRotateDeg(Vec(-1,0).signedDeg(orbit.f2-orbit.f1))
                      drawEllipse(DVec.zero, orbit.a * scale, orbit.b * scale, RED)
                    }
                  case None =>
                }
              case None =>

            }
          case None =>
            continueFutureTrajectory()
        }
      }

      drawFilledCircle(station.coord*scale, earth.radius * scale / 2f / globalScale, WHITE)
      orbitAroundCelestialInPointWithVelocity(station.mass, station.coord, station.linearVelocity) match {
        case Some((planet, orbit)) =>
          openglLocalTransform {
            openglMove(orbit.center*scale)
            openglRotateDeg(Vec(-1,0).signedDeg(orbit.f2-orbit.f1))
            drawEllipse(DVec.zero, orbit.a * scale, orbit.b * scale, YELLOW)
          }
        case None =>
      }

      for {
        x <- left_up_corner
        y <- right_down_corner
      } {
        val c = (y-x).n*(y.dist(x)/2f)+x
        val w = math.abs(y.x - x.x)
        val h = math.abs(y.y - x.y)

        drawRectCentered(c,w,h,DARK_GRAY)
      }
    } else {
      val m = absCoord(mouseCoord)
      val d = ship.coord.dist(m)
      openglLocalTransform {
        openglMove(ship.coord - base)
        drawArrow(DVec.zero, m-ship.coord, DARK_GRAY)
        openglMove(m-ship.coord)
        openglRotateDeg(-rotationAngleDeg)
        print(s"  ${mOrKm(d.toLong)}", DVec.zero, size = max_font_size / globalScale, DARK_GRAY)
      }

    }
  }

  interface {
    if(onPause) print("Пауза", windowCenter, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$ticks", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec*fps/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%/${1*averageActionTimeMsec*ticks/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%", windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec", windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec", windowWidth - 20, windowHeight - 100, align = "top-right", color = DARK_GRAY)

    val a = DVec(windowWidth - 250, 20)
    val b = DVec(windowWidth - 250 + 100, 20)
    drawLine(a,b, DARK_GRAY)
    drawLine(a, a+(a-b).rotateDeg(90).n*5, DARK_GRAY)
    drawLine(b, b+(a-b).rotateDeg(90).n*5, DARK_GRAY)
    print(s"${mOrKm((100/globalScale/(if(drawMapMode) scale else 1.0)).toInt)}", b, DARK_GRAY)

    if(!disable_interface_drawing) {
      openglLocalTransform {
        val z = windowHeight/2f-40f
        openglMove(windowCenter)
        //openglRotateDeg(rotationAngleDeg)
        drawArrow(DVec(0, -z), DVec(0, z), DARK_GRAY, 1)
        print("y", DVec(0, z), DARK_GRAY)
        drawArrow(DVec(-z, 0), DVec(z, 0), DARK_GRAY, 1)
        print("x", DVec(z, 0), DARK_GRAY)
      }

      val heights = (400 to 20 by -20).iterator

      print(s"Время: ${timeStr((_tacts*base_dt*1000).toLong)}",
        20, heights.next(), ORANGE)
      print(f"Ускорение времени: x${timeMultiplier*k}%.2f",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      print(s"Режим камеры: $viewModeStr",
        20, heights.next(), ORANGE)
      print(s"Полетный режим: ${ship.flightModeStr}",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      /*print(f"Расстояние и скорость относительно Земли: ${mOrKm(ship.coord.dist(earth.coord) - earth.radius)}, ${msecOrKmsec(ship.linearVelocity*(ship.coord - earth.coord).n)}",
        20, heights.next(), ORANGE)
      print(f"Расстояние и скорость относительно Луны: ${mOrKm(ship.coord.dist(moon.coord) - moon.radius)}, ${msecOrKmsec(ship.linearVelocity* (ship.coord - moon.coord).n)}",
        20, heights.next(), ORANGE)*/
      /*print(s"Расчет траектории: ${if(continue_future_trajectory) "[rактивирован]" else "отключен"}",
        20, heights.next(), ORANGE)*/

      //print("", 20, heights.next(), ORANGE)

      /*print(f"Позиция: ${ship.coord.x}%.2f : ${ship.coord.y}%.2f. Расстояние от точки старта: ${mOrKm(ship.coord.dist(ship_start_position))}",
        20, heights.next(), ORANGE)*/
      print(f"Линейная скорость: ${msecOrKmsec(ship.linearVelocity.norma)} (velx = ${msecOrKmsec(ship.linearVelocity.x)}, vely = ${msecOrKmsec(ship.linearVelocity.y)})",
        20, heights.next(), ORANGE)
      /*print(f"Угол: ${ship.rotation}%.2f град",
        20, heights.next(), ORANGE)*/
      print(f"Угловая скорость: ${ship.angularVelocity}%.2f град/сек",
        20, heights.next(), ORANGE)
      print(s"Параметры орбиты: ${orbitStrInPointWithVelocity(ship.mass, ship.coord, ship.linearVelocity)}",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      print(f"Скорость для спутника в данной точке: ${satelliteSpeedStrInPoint(ship.coord)}",
        20, heights.next(), ORANGE)
      print(f"Скорость убегания в данной точке: ${escapeVelocityStrInPoint(ship.coord)}",
        20, heights.next(), ORANGE)

      print("", 20, heights.next(), ORANGE)

      print(s"Двигательная установка: ${if(ship.engines.exists(_.active)) "[rактивирована]" else "отключена"}",
        20, heights.next(), ORANGE)
      print(s"Мощность и время работы отдельных двигателей:",
        20, heights.next(), ORANGE)
      print(s"${ship.engines.map(e => {
        if(ship.selected_engine.exists(x => x == e)) s"[r${e.powerPercent} % (${e.worktimeTacts} т.)]"
        else s"${e.powerPercent} % (${e.worktimeTacts} т.)"
      }).mkString(", ")}",
        20, heights.next()
        , ORANGE)
      print(f"Линейная скорость в момент отключения двигателей: $linearSpeedStrWhenEnginesOff",
        20, heights.next(), ORANGE)
      print(f"Угловая скорость в момент отключения двигателей: $angularSpeedStrWhenEnginesOff",
        20, heights.next(), ORANGE)
      print(s"Параметры орбиты в момент отключения двигателей: $orbitParametersStrWhenEnginesOff",
        20, heights.next(), ORANGE)
    }
  }

  pause()
}

import OrbitalKiller._

trait CelestialBody {
  def index:String
  def coord:DVec
  def linearVelocity:DVec
  def mass:Double
  def radius:Double
}

class Planet(
  val index:String,
  val mass:Double,
  val init_coord:DVec,
  val init_velocity:DVec,
  val radius:Double) extends CelestialBody {
  lazy val gravitational_radius = {
    coord.dist(earth.coord) * math.sqrt(mass / 20.0 / earth.mass) / (1.0 + math.sqrt(mass / 20.0 / earth.mass))
  }

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0,
      ang_vel = 0,
      ang = 0,
      shape = CircleShape(radius),
      is_static = false))

  render {
    val viewpoint_dist = math.abs(ship.coord.dist(coord) - radius)
    if(viewpoint_dist < 50000) {
      openglLocalTransform {
        openglMove(coord - base)
        val to_viewpoint = ship.coord - coord
        val alpha = 100000 * 180 / math.Pi / radius
        val points = for {
          ang <- -alpha to alpha by 0.01
          point = to_viewpoint.rotateDeg(ang).n * radius
        } yield point
        drawSlidingLines(points, WHITE)

        /*val x = (ship.coord-coord).n*radius
        val p1 = x + (ship.coord-coord).rotateDeg(90).n*60000
        val p2 = x + (ship.coord-coord).rotateDeg(-90).n*60000
        drawLine(p1,p2,GREEN)*/
      }
    }
  }
}

class Star(val index:String, val mass:Double, val coord:DVec, val radius:Double) extends CelestialBody {
  lazy val gravitational_radius = {
    coord.dist(moon.coord) * math.sqrt(mass / 20 / moon.mass) / (1 + math.sqrt(mass / 20 / moon.mass))
    /*radius + 10000000*/
  }

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = DVec.dzero,
      vel = DVec.dzero,
      coord,
      ang_acc = 0,
      ang_vel = 0,
      ang = 0,
      shape = CircleShape(radius),
      is_static = true))

  render {
    val viewpoint_dist = math.abs(ship.coord.dist(coord) - radius)
    if(viewpoint_dist < 50000) {
      openglLocalTransform {
        openglMove(coord)
        val to_viewpoint = ship.coord - coord
        val alpha = 100000 * 180 / math.Pi / radius
        val points = for {
          ang <- -alpha to alpha by 0.01
          point = to_viewpoint.rotateDeg(ang).n * radius
        } yield point
        drawSlidingLines(points, WHITE)

        /*val x = (ship.coord-coord).n*radius
        val p1 = x + (ship.coord-coord).rotateDeg(90).n*60000
        val p2 = x + (ship.coord-coord).rotateDeg(-90).n*60000
        drawLine(p1,p2,GREEN)*/
      }
    }
  }


  def linearVelocity: DVec = DVec.dzero
}






