package com.github.dunnololda.scageprojects.orbitalkiller

import java.io.FileOutputStream

import com.github.dunnololda.scage.ScageLibD._

import scala.collection.mutable.ArrayBuffer

//import collection.mutable.ArrayBuffer
import scala.collection.mutable

// TODO: implement this
sealed trait ViewMode
sealed trait FlightMode

object OrbitalKiller extends ScageScreenAppD("Orbital Killer", 1280, 768) {
  val k:Double = 1 // доля секунды симуляции, которая обрабатывается за одну реальную секунду, если не применяется ускорение

  // движок делает вызов обработчика примерно 60 раз в секунду, за каждый вызов будет обрабатывать вот такую порцию симуляции
  // то есть, мы хотим, чтобы за одну реальную секунду обрабатывалось k секунд симуляции, поэтому за один такт движка (которых 60 в секунду)
  // будем обрабатывать k/60
  val base_dt:Double = 1.0/63*k

  val realtime = (1.0/k).toInt // 1/k соответствует реальному течению времени

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
          e.workTimeTacts = e.workTimeTacts
        })

      }
    }
  }

  def maxTimeMultiplier:Int = {
    /*val a = ships.map(s => math.abs(s.currentState.acc*s.currentState.vel.p)).max
    math.max((0.1*math.pow(351.3011068768212/a, 10.0/7)/5).toInt, 1)*/
    1
  }

  def dt = timeMultiplier*base_dt

  private var _tacts = 0l
  def tacts:Long = _tacts

  private val current_body_states = mutable.HashMap[String, BodyState]()
  def currentBodyState(index:String):Option[BodyState] = current_body_states.get(index)
  def currentSystemState = current_body_states.values.toList

  def futureSystemEvolutionFrom(dt: => Double, tacts:Long, body_states:List[BodyState], enable_collisions:Boolean) = systemEvolutionFrom(
    dt, maxTimeMultiplier, base_dt,
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
    changeFunction =  (time, bodies) => {
      (time, bodies.map {
        case b =>
          // зануляем угловую скорость, если она меньше 0.01
          if(b.ang_vel != 0 && math.abs(b.ang_vel) < 0.01 && ship_indices.contains(b.index)) b.copy(ang_vel = 0)
          else b
      })
    },
    enable_collisions = enable_collisions)((tacts, body_states))

  def futureSystemEvolutionWithoutReactiveForcesFrom(dt: => Double, tacts:Long, body_states:List[BodyState], enable_collisions:Boolean) = systemEvolutionFrom(
    dt, Int.MaxValue, base_dt,
    force = (tacts, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
          other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero)
        case station.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
          other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero)
        case moon.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G)
        case _ => DVec.dzero
      }
    },
    torque = (tacts, bs, other_bodies) => 0,
    enable_collisions = enable_collisions)((tacts, body_states))

  def futureSystemEvolutionWithCustomMaxMultiplierFrom(dt: => Double, max_multiplier:Int, tacts:Long, body_states:List[BodyState], enable_collisions:Boolean) = systemEvolutionFrom(
    dt, max_multiplier, base_dt,
    force = (tacts, bs, other_bodies) => {
      bs.index match {
        case ship.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
            other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero)
        case station.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
            other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero)
        case moon.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G)
        case _ => DVec.dzero
      }
    },
    torque = (tacts, bs, other_bodies) => 0,
    enable_collisions = enable_collisions)((tacts, body_states))

  //private var continue_future_trajectory = false
  //val trajectory_accuracy = 100 // для рисования: рисуем только каждую сотую точку траектории
  //val trajectory_capacity = 100000

  private val future_trajectory = ArrayBuffer[(Long, List[BodyState])]()
  //private val future_trajectory_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()
  private val future_trajectory_capacity = 10000

  def getFutureState(tacts:Long):List[BodyState] = {
    if(ship.flightMode != 0) {
      if (future_trajectory.length >= tacts) future_trajectory(tacts.toInt)._2
      else {
        continueFutureTrajectory(s"getFutureState($tacts)")
        getFutureState(tacts)
      }
    } else Nil
  }

  //private val body_trajectories_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()

  def updateFutureTrajectory(reason:String) {
    if(ship.flightMode != 0) {
      println(s"updateFutureTrajectory: $reason")
      future_trajectory.clear()
      future_trajectory ++= {
        futureSystemEvolutionFrom(base_dt, _tacts, currentSystemState, enable_collisions = false)
          .take(future_trajectory_capacity)
      }
      _calculate_orbits = true
      //updateFutureTrajectoryMap()
    }
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

  def continueFutureTrajectory(reason:String) {
    if(ship.flightMode != 0) {
      println(s"continueFutureTrajectory: $reason")
      val (t, s) = future_trajectory.lastOption.getOrElse((_tacts, currentSystemState))
      val steps = {
        futureSystemEvolutionFrom(base_dt, t, s, enable_collisions = false)
          .take(future_trajectory_capacity)
      }.toSeq
      future_trajectory ++= steps
      //continueFutureTrajectoryMap(steps)
    }
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

  val earth = new Star("Earth", mass = 5.9746E24, coord = DVec.dzero, ang_vel = 360.0/(24l*60*60), radius = 6400000)

  val moon_start_position = DVec(-269000000, 269000000)
  val moon_init_velocity = satelliteSpeed(moon_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)
  val moon = new Planet(
    "Moon",
    mass = 7.3477E22,
    init_coord = moon_start_position,
    init_velocity = moon_init_velocity,
    init_ang_vel = 360.0/(26l*24*60*60 + 8l*60*60 + 59l*60 + 44),   // период орбиты луны в данной симуляции: 26 д. 8 ч. 59 мин. 44 сек, равен периоду обращения вокруг собственной оси
    radius = 1737000)

  val planets = List(earth, moon)
  val planet_indexes = planets.map(_.index).toSet
  def currentPlanetStates = planets.map(_.currentState)
  def planetByIndex(index:String):Option[CelestialBody] = planets.find(_.index == index)

  val ship_start_position = earth.coord + DVec(0, earth.radius + 500)
  val ship_init_velocity = DVec.zero/*satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.15*/
  //val ship_init_velocity = -escapeVelocity(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.01
  //val ship_start_position = moon.coord + DVec(0, moon.radius + 100000)
  //val ship_init_velocity = satelliteSpeed(ship_start_position, moon.coord, moon.linearVelocity, moon.mass, G, counterclockwise = false)* 1.15
  val ship = new Ship3("ship",
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 0
  )

  val station_start_position = earth.coord + DVec(300, earth.radius + 300000)
  val station_init_velocity = satelliteSpeed(station_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.15
  val station = new SpaceStation("station",
    init_coord = station_start_position,
    init_velocity = station_init_velocity,
    init_rotation = 45
  )
  
  val ships = List(ship, station)
  val ship_indices = ships.map(_.index).toSet

  private var real_system_evolution =
    futureSystemEvolutionFrom(dt, 0, List(
        ship.currentState,
        station.currentState,
        moon.currentState,
        earth.currentState),
      enable_collisions = true).iterator

  private var _stop_after_number_of_tacts:Long = 0//56700

  //private var skipped_points = 0
  private var ship_states_with_different_max_multipliers:List[(Int, Double)] = Nil
  private val xx = 250
  private val fos = new FileOutputStream(s"errors_stats_$xx.txt", true)
  clear {
    fos.close()
  }
  private def nextStep() {
    val (t, body_states) = real_system_evolution.next()
    if(_stop_after_number_of_tacts > 0) {
      _stop_after_number_of_tacts -= (t - _tacts)
      if(_stop_after_number_of_tacts <= 0) {
        if (timeMultiplier != realtime) {
          timeMultiplier = realtime
        }
        pause()
      } else if(_stop_after_number_of_tacts < timeMultiplier) {
        timeMultiplier = math.max((_stop_after_number_of_tacts/2).toInt, 1)
      }
    }

    if(timeMultiplier == xx) {
      val system_state = currentSystemState
      val mults = List(50,100,150,200,250,300,350,400).takeWhile(x => x <= timeMultiplier)
      ship_states_with_different_max_multipliers = mults.flatMap {
        case max_multiplier =>
          val (t, diff_body_states) = futureSystemEvolutionWithCustomMaxMultiplierFrom(dt, max_multiplier, _tacts, system_state, enable_collisions = true).head
          for {
            real_ship <- body_states.find(_.index == ship.index)
            diff_ship <- diff_body_states.find(_.index == ship.index)
          } yield (max_multiplier, real_ship.coord.dist(diff_ship.coord))
      }
      //fos.write(s"${mOrKm(earth.coord.dist(ship.coord))} : ${mOrKm(moon.coord.dist(ship.coord))} : ${msecOrKmsec(ship.linearVelocity.norma)} : ${curvatureRadiusStrInPoint(ship.currentState)} : ${ship_states_with_different_max_multipliers.map(x => s"${x._1} ${mOrKm(x._2)}").mkString(" : ")}\n".getBytes)
      ship_states_with_different_max_multipliers.find(_._1 == xx).foreach(x => fos.write(s"${ship.linearAcceleration.norma} ${x._2}\n".getBytes))
    } else ship_states_with_different_max_multipliers = Nil

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
      case 3 => // фиксация на корабле, абсолютная ориентация
        center = ship.coord + _ship_offset
        base = if(ship.coord.norma < 100000) DVec.zero else ship.coord
        rotationAngle = 0
        view_mode = 3
      case 4 => // в режиме карты зафиксировать центр орбиты в центре экрана
        if(drawMapMode) {
          center = orbitAroundCelestialInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass, currentPlanetStates).map(_._2.center * scale).getOrElse(ship.coord)
          rotationAngle = 0
          view_mode = 4
        }
      case _ =>
    }
  }
  private def viewModeStr = view_mode match {
    case 0 => "свободный"
    case 1 => "фиксация на корабле"
    case 2 => "посадка"
    case 3 => "фиксация на корабле, абсолютная ориентация"
    case 4 => "фиксация на орбите корабля"
    case _ => ""
  }

  def satelliteSpeedStrInPoint(coord:DVec, velocity:DVec, mass:Double):String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val ss = satelliteSpeed(coord, velocity, planet_state.coord, planet_state.vel, planet_state.mass, G)
        f"${msecOrKmsec(ss.norma)} (velx = ${msecOrKmsec(ss.x)}, vely = ${msecOrKmsec(ss.y)})"
      case None =>
        "N/A"
    }
  }

  def escapeVelocityStrInPoint(coord:DVec, velocity:DVec, mass:Double):String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        val ss = escapeVelocity(coord, velocity, planet_state.coord, planet_state.vel, planet_state.mass, G)
        f"${msecOrKmsec(ss.norma)} (velx = ${msecOrKmsec(ss.x)}, vely = ${msecOrKmsec(ss.y)})"
      case None =>
        "N/A"
    }
  }

  def curvatureRadiusInPoint(body_state:BodyState):Double = {
    math.abs(body_state.vel.norma2/(body_state.acc * body_state.vel.p))
  }

  def curvatureRadiusStrInPoint(body_state:BodyState):String = {
    s"${mOrKm(curvatureRadiusInPoint(body_state))}"
  }

  def orbitStrInPointWithVelocity(coord:DVec, velocity:DVec, mass:Double, planet_states:Seq[BodyState]):String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, planet_states) match {
      case Some((planet, planet_state)) =>
        val prefix = planet.index match {
          case earth.index => "Земля"
          case moon.index => "Луна"
        }
        val orbit = calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)
        orbit.strDefinition(prefix, planetByIndex(planet_state.index).get.radius)
      case None => "N/A"
    }
  }

  def orbitInPointWithVelocity(coord:DVec, velocity:DVec, mass:Double):Option[KeplerOrbit] = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        Some(calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G))
      case None => None
    }
  }

  def orbitAroundCelestialInPointWithVelocity(coord:DVec, velocity:DVec, mass:Double, planet_states:Seq[BodyState]):Option[((CelestialBody, BodyState), KeplerOrbit)] = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        planetByIndex(planet_state.index).flatMap(planet => {
          Some(((planet, planet_state), calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)))
        })
      case None => None
    }
  }

  def linearSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
          case Some((t, lbs)) =>
            lbs.find(_.index == ship.index) match {
              case Some(bs) =>
                val s = bs.vel
                //f"${msecOrKmsec(s.norma)} (velx = ${msecOrKmsec(s.x)}, vely = ${msecOrKmsec(s.y)})"
                f"${msecOrKmsec(s.norma)}"
              case None => "N/A"
            }
          case None =>
            continueFutureTrajectory("linearSpeedStrWhenEnginesOff")
            "N/A"
        }
      } else {
        //f"${msecOrKmsec(ship.linearVelocity.norma)} (velx = ${msecOrKmsec(ship.linearVelocity.x)}, vely = ${msecOrKmsec(ship.linearVelocity.y)})"
        f"${msecOrKmsec(ship.linearVelocity.norma)}"
      }
    }
  }

  def angularSpeedStrWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
          case Some((t, lbs)) =>
            lbs.find(_.index == ship.index) match {
              case Some(bs) =>
                val s = bs.ang_vel
                f" $s%.2f град/сек"
              case None => "N/A"
            }
          case None =>
            continueFutureTrajectory("angularSpeedStrWhenEnginesOff")
            "N/A"
        }
      } else {
        f" ${ship.angularVelocity}%.2f град/сек"
      }
    }
  }

  def orbitParametersStrWhenEnginesOff:String = {
    if(ship.flightMode != 1) "N/A"  // только в свободном режиме отображать инфу
    else {
      if(ship.engines.exists(_.active)) {
        future_trajectory.find(_._1 >= ship.engines.map(_.stopMomentTacts).max) match {
          case Some((t, lbs)) =>
            lbs.find(_.index == ship.index) match {
              case Some(bs) =>
                orbitStrInPointWithVelocity(bs.coord, bs.vel, bs.mass, lbs.filter(x => planet_indexes.contains(x.index)))
              case None =>"N/A"
            }
          case None =>
            continueFutureTrajectory("orbitParametersStrWhenEnginesOff")
            "N/A"
        }
      } else {
        orbitStrInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass, currentSystemState.filter(x => planet_indexes.contains(x.index)))
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
      orbitAroundCelestialInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass, currentPlanetStates) match {
        case Some((planet, kepler_orbit)) =>
          kepler_orbit match {
            case ellipse:EllipseOrbit =>
              val b = BoxShape(2*ellipse.a, 2*ellipse.b)
              val aabb = b.aabb(ellipse.center, Vec(-1,0).signedDeg(ellipse.f2-ellipse.f))
              viewMode = 0
              globalScale = 750 / (aabb.height * scale)
              _center = ellipse.center*scale
            case hyperbola:HyperbolaOrbit =>
              val b = BoxShape(2*hyperbola.a, 2*hyperbola.b)
              val aabb = b.aabb(hyperbola.center, Vec(-1,0).signedDeg(hyperbola.center-hyperbola.f))
              viewMode = 0
              globalScale = 750 / (aabb.height * scale)
              _center = hyperbola.center*scale
          }
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

  def insideSphereOfInfluenceOfCelestialBody(ship_coord:DVec, ship_mass:Double, planet_states:Seq[BodyState]):Option[(CelestialBody, BodyState)] = {
    for {
      moon_state <- planet_states.find(_.index == moon.index)
      earth_state <- planet_states.find(_.index == earth.index)
    } yield {
      val earth_force = gravityForce(earth_state.coord, earth_state.mass, ship_coord, ship_mass, G).norma
      val moon_force = gravityForce(moon_state.coord, moon_state.mass, ship_coord, ship_mass, G).norma
      if(earth_force >= moon_force) (earth, earth_state) else (moon, moon_state)
      /*if(coord.dist(moon_state.coord) < soi(moon_state.mass, moon_state.coord.dist(earth_state.coord), earth_state.mass)) {
        (moon, moon_state)
      } else (earth, earth_state)*/
    }
  }

  def drawArrow(from1:DVec, to1:DVec, color:ScageColor, scale:Double = globalScale) {
    val arrow11 = to1 + ((from1-to1).n*10/scale).rotateDeg(15)
    val arrow12 = to1 + ((from1-to1).n*10/scale).rotateDeg(-15)
    drawLine(from1, to1, color)
    drawLine(to1, arrow11, color)
    drawLine(to1, arrow12, color)
  }

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD1)})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD2)})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD3)})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD4)})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD6)})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD7)})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD8)})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {ship.switchEngineActiveOrSelect(KEY_NUMPAD9)})

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {ship.engines.foreach(e => {
    if(e.active || e.power > 0) {
      e.active = false
      e.power = 0
    } else {
      e.workTimeTacts = 0
    }
  })})
  keyIgnorePause(KEY_NUMPAD0, onKeyDown = {ship.engines.foreach(e => {
    e.workTimeTacts = 226800 // 1 hour
  })})

  private def repeatTime(code:Int):Long = {
    keyPress(code).map {
      case kp =>
        if(kp.was_pressed && System.currentTimeMillis() - kp.pressed_start_time < 100) 100l
        else 10l
    }.getOrElse(100l)
  }

  keyIgnorePause(KEY_UP,      repeatTime(KEY_UP),    onKeyDown = {ship.selected_engine.foreach(e => e.powerPercent += 1)}, onKeyUp = updateFutureTrajectory("KEY_UP"))
  keyIgnorePause(KEY_DOWN,    repeatTime(KEY_DOWN),  onKeyDown = {ship.selected_engine.foreach(e => e.powerPercent -= 1)}, onKeyUp = updateFutureTrajectory("KEY_DOWN"))
  keyIgnorePause(KEY_RIGHT,   repeatTime(KEY_RIGHT), onKeyDown = {
    ship.selected_engine.foreach(e => {
      e.workTimeTacts += 1
      //updateFutureTrajectory()
    })
  }, onKeyUp = updateFutureTrajectory("KEY_RIGHT"))
  keyIgnorePause(KEY_LEFT,    repeatTime(KEY_LEFT),  onKeyDown = {
    ship.selected_engine.foreach(e => {
      /*if(e.worktimeTacts > 0) {*/
        e.workTimeTacts -= 1
        //updateFutureTrajectory()
      /*}*/
    })
  }, onKeyUp = updateFutureTrajectory("KEY_LEFT"))

  keyIgnorePause(KEY_ADD, 100, onKeyDown = {
    timeMultiplier += realtime
  }, onKeyUp = updateFutureTrajectory("KEY_ADD"))
  keyIgnorePause(KEY_SUBTRACT, 100, onKeyDown = {
    if(timeMultiplier > realtime) {
      timeMultiplier -= realtime
    }
  }, onKeyUp = updateFutureTrajectory("KEY_SUBTRACT"))

  keyIgnorePause(KEY_MULTIPLY, 100, onKeyDown = {
    if(timeMultiplier == realtime) {
      timeMultiplier = realtime*50
    } else {
      timeMultiplier += realtime*50
    }
  }, onKeyUp = updateFutureTrajectory("KEY_MULTIPLY"))
  keyIgnorePause(KEY_DIVIDE, 100, onKeyDown = {
    if (timeMultiplier != realtime) {
      timeMultiplier = realtime
    }
  }, onKeyUp = updateFutureTrajectory("KEY_DIVIDE"))

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
  keyIgnorePause(KEY_8, onKeyDown = ship.flightMode = 8)
  keyIgnorePause(KEY_9, onKeyDown = ship.flightMode = 9)
  keyIgnorePause(KEY_0, onKeyDown = ship.flightMode = 0)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {pause(); holdCounters {HelpScreen.run()}})
  keyIgnorePause(KEY_F2, onKeyDown = if(!drawMapMode) viewMode = 1 else viewMode = 0)     // фиксация на корабле, в режиме карты: свободный режим
  keyIgnorePause(KEY_F3, onKeyDown = if(!drawMapMode) viewMode = 3 else viewMode = 4)     // фиксация на корабле, абсолютная ориентация, в режиме карты: фиксация на орбите
  keyIgnorePause(KEY_F4, onKeyDown = if(!drawMapMode) viewMode = 2)                       // посадка на планету, если не в режиме карты

  keyIgnorePause(KEY_F5, onKeyDown = saveGame())                          // сохранить текущее состояние системы
  keyIgnorePause(KEY_F6, onKeyDown = loadGame())                          // загрузить из файла состояние системы

  private var _show_game_saved_message = false
  def saveGame() {
    val fos = new FileOutputStream("save.orbitalkiller")
    fos.write(s"time ${_tacts}\n".getBytes)
    currentSystemState.filter(_.index == "ship").foreach {
      case BodyState(index, _, acc, vel, coord, ang_acc, ang_vel, ang, _, _, _, _, _) =>
        fos.write(s"$index ${acc.x}:${acc.y} ${vel.x}:${vel.y} ${coord.x}:${coord.y} $ang_acc $ang_vel $ang\n".getBytes)
    }
    currentSystemState.filter(_.index == "station").foreach {
      case BodyState(index, _, acc, vel, coord, ang_acc, ang_vel, ang, _, _, _, _, _) =>
        fos.write(s"$index ${acc.x}:${acc.y} ${vel.x}:${vel.y} ${coord.x}:${coord.y} $ang_acc $ang_vel $ang\n".getBytes)
    }
    currentSystemState.filter(_.index == "Moon").foreach {
      case BodyState(index, _, acc, vel, coord, _, _, _, _, _, _, _, _) =>
        fos.write(s"$index ${acc.x}:${acc.y} ${vel.x}:${vel.y} ${coord.x}:${coord.y} 0 0 0\n".getBytes)
    }
    fos.close()
    _show_game_saved_message = true
    val start = System.currentTimeMillis()
    actionIgnorePause(1000) {
      if(System.currentTimeMillis() - start > 2000) {
        _show_game_saved_message = false
        deleteSelf()
      }
    }
  }

  private var _show_game_loaded_message = false
  private var _show_game_failed_to_load_message = false
  def loadGame() {
    def _parseDVec(str:String):Option[DVec] = {
      val s = str.split(":")
      if(s.length == 2) {
        try {
          Some(DVec(s(0).toDouble, s(1).toDouble))
        } catch {
          case e:Exception => None
        }
      } else None
    }

    def _parseLong(str:String):Option[Long] = {
      try {
        Some(str.toLong)
      } catch {
        case e: Exception => None
      }
    }

    def _parseDouble(str:String):Option[Double] = {
      try {
        Some(str.toDouble)
      } catch {
        case e: Exception => None
      }
    }

    val savefile_lines = io.Source.fromFile("save.orbitalkiller").getLines().toList
    val new_tacts_option = savefile_lines.headOption.flatMap(l => {
      val s = l.split(" ")
      if(s.length == 2)_parseLong(s(1)) else None
    })

    val new_states = (for {
      line <- savefile_lines.drop(1)
      s = line.split(" ")
      if s.length == 7
      index = s(0)
      acc <- _parseDVec(s(1))
      vel <- _parseDVec(s(2))
      coord <- _parseDVec(s(3))
      ang_acc <- _parseDouble(s(4))
      ang_vel <- _parseDouble(s(4))
      ang <- _parseDouble(s(4))
    } yield {
      (index, (acc, vel, coord, ang_acc, ang_vel, ang))
    }).toMap

    for {
      new_tacts <- new_tacts_option
      ship_state <- new_states.get("ship")
      (ship_acc, ship_vel, ship_coord, ship_ang_acc, ship_ang_vel, ship_ang) = ship_state
      station_state <- new_states.get("station")
      (station_acc, station_vel, station_coord, station_ang_acc, station_ang_vel, station_ang) = station_state
      moon_state <- new_states.get("Moon")
      (moon_acc, moon_vel, moon_coord, _, _, _) = moon_state
    } {
      _tacts = new_tacts
      real_system_evolution = futureSystemEvolutionFrom(dt, _tacts, List(
          ship.currentState.copy(acc = ship_acc, vel = ship_vel, coord = ship_coord, ang_acc = ship_ang_acc, ang_vel = ship_ang_vel, ang = ship_ang),
          station.currentState.copy(acc = station_acc, vel = station_vel, coord = station_coord, ang_acc = station_ang_acc, ang_vel = station_ang_vel, ang = station_ang),
          moon.currentState.copy(acc = moon_acc, vel = moon_vel, coord = moon_coord),
          earth.currentState),
          enable_collisions = true).iterator
      _show_game_loaded_message = true
    }
    if(_show_game_loaded_message) {
      val start = System.currentTimeMillis()
      actionIgnorePause(1000) {
        if(System.currentTimeMillis() - start > 2000) {
          _show_game_loaded_message = false
          deleteSelf()
        }
      }
    } else {
      _show_game_failed_to_load_message = true
      val start = System.currentTimeMillis()
      actionIgnorePause(1000) {
        if(System.currentTimeMillis() - start > 2000) {
          _show_game_failed_to_load_message = false
          deleteSelf()
        }
      }
    }
  }

  /*keyIgnorePause(KEY_Z, onKeyDown = disable_trajectory_drawing = !disable_trajectory_drawing)
  keyIgnorePause(KEY_X, onKeyDown = disable_future_trajectory_drawing = !disable_future_trajectory_drawing)*/
  keyIgnorePause(KEY_I, onKeyDown = disable_interface_drawing = !disable_interface_drawing)
  keyIgnorePause(KEY_R, onKeyDown = _rendering_enabled = !_rendering_enabled)
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
    println(globalScale)
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
    println(globalScale)
  })


  private var left_up_corner:Option[DVec] = None
  private var right_down_corner:Option[DVec] = None
  leftMouseIgnorePause(onBtnDown = m => {if(drawMapMode) left_up_corner = Some(absCoord(m))}, onBtnUp = m => {
    if(drawMapMode) {
      if(right_down_corner.nonEmpty) {
        for {
          x <- left_up_corner
          y <- right_down_corner
          c = (y - x).n * (y.dist(x) / 2f) + x
          h = math.abs(y.y - x.y)
          if h*globalScale > 10
        } {
          globalScale = 750 / h
          _center = c
        }
        left_up_corner = None
        right_down_corner = None
      } else {

      }
    }})
  leftMouseDragIgnorePause(onDrag = m => if(drawMapMode) {
    right_down_corner = Some(absCoord(m))
  })

  actionIgnorePause {
    //if(future_trajectory.isEmpty) updateFutureTrajectory("future_trajectory.isEmpty")
  }

  /*actionIgnorePause(100) {
    if(continue_future_trajectory) continueFutureTrajectory()
  }*/

  action {
    future_trajectory --= future_trajectory.takeWhile(_._1 < _tacts)
    //future_trajectory_map.values.foreach(t => t --= t.takeWhile(_._1 < _tacts))
    //if(future_trajectory.isEmpty) updateFutureTrajectory()
    nextStep()
    //timeMultiplier = maxTimeMultiplier
  }

  private var _center = ship.coord
  private var _ship_offset = DVec.zero
  def shipOffset = _ship_offset
  center = _center
  windowCenter = DVec((windowWidth-1024)+1024/2, windowHeight/2)
  viewMode = 1

  private val scale = 1e-6

  private def reducePointsNumber(points:Seq[DVec], res:List[DVec] = Nil):List[DVec] = {
    if(points.isEmpty) res
    else {
      val p1 = points.head
      if(points.tail.isEmpty) res ::: List(p1)
      else {
        if(res.isEmpty) reducePointsNumber(points.tail, List(p1))
        else {
          val p0 = res.last
          val next_points = points.tail.dropWhile(p2 => (p2-p1).deg(p1-p0) < 5)
          reducePointsNumber(next_points, res ::: List(p1))
        }
      }
    }
  }

  private def shipOrbitRender(ship_index:String, some_system_state:List[BodyState], color1:ScageColor, color2:ScageColor):() => Unit = {
    val result = ArrayBuffer[() => Unit]()
    // находим наш корабль
    some_system_state.find(_.index == ship_index) match {
      case Some(bs) =>
        // смотрим, где он находится
        insideSphereOfInfluenceOfCelestialBody(bs.coord, bs.mass, some_system_state.filter(x => planet_indexes.contains(x.index))) match {
          case Some((planet, planet_state)) =>
            // корабль находится внутри гравитационного радиуса какой-то планеты (земли или луны)
            val orbit = calculateOrbit(
              planet_state.mass,
              planet_state.coord,
              bs.mass,
              bs.coord - planet_state.coord,
              bs.vel - planet_state.vel, G)
            orbit match {
              case hyperbola:HyperbolaOrbit =>
                val axis = (hyperbola.center - hyperbola.f).n
                val yy = (-math.acos(-1.0/hyperbola.e)+0.1 to math.acos(-1.0/hyperbola.e)-0.1 by 0.1).map(true_anomaly => {
                  val r = hyperbola.a*(hyperbola.e*hyperbola.e-1)/(1 + hyperbola.e*math.cos(true_anomaly))
                  (hyperbola.f + (axis*r).rotateRad(true_anomaly))*scale
                }).toList
                result += (() => {
                  drawSlidingLines(yy, color1)
                })
              case ellipse:EllipseOrbit =>
                result += (() => openglLocalTransform {
                  openglMove(ellipse.center*scale)
                  openglRotateDeg(Vec(-1,0).signedDeg(ellipse.f2-ellipse.f))
                  drawEllipse(DVec.zero, ellipse.a * scale, ellipse.b * scale, color2)
                })
            }
          case None =>
        }
      case None =>
    }
    () => result.foreach(x => x())
  }

  private var ship_orbit_render:() => Unit = () => {}
  private var station_orbit_render:() => Unit = () => {}

  private var _calculate_orbits = false

  private var _rendering_enabled = true
  def renderingEnabled = _rendering_enabled

  private def updateOrbits() {
    ship_orbit_render = if(ship.engines.exists(_.active)) {
      // получаем состояние системы, когда двигатели отработали
      val system_state_when_engines_off = getFutureState(ships.flatMap(_.engines.map(e => e.workTimeTacts)).max)
      shipOrbitRender(ship.index, system_state_when_engines_off, PURPLE, RED)
    } else {
      // двигатели корабля не работают - можно работать с текущим состоянием
      shipOrbitRender(ship.index, currentSystemState, ORANGE, YELLOW)
      /*val one_week_evolution_after_engines_off = futureSystemEvolutionWithoutReactiveForcesFrom(
        3600 * base_dt, _tacts, currentSystemState, enable_collisions = false)
        .take(7* 24 * 63)
        .zipWithIndex
        .filter(_._2 % 5 == 0)
        .map(_._1)
        .map(_._2)
        .toList
      drawSlidingLines(one_week_evolution_after_engines_off.flatMap(x => x.find(_.index == ship.index)).map(_.coord * scale), ORANGE)*/
    }
    station_orbit_render = shipOrbitRender(station.index, currentSystemState, ORANGE, YELLOW)
    _calculate_orbits = false
  }
  updateOrbits()

  actionIgnorePause(1000) {
    if(_rendering_enabled && drawMapMode && (!onPause || _calculate_orbits)) {
      updateOrbits()
    }
  }

  render {
    if(_rendering_enabled) {
      if(drawMapMode) {
        drawCircle(earth.coord*scale, earth.radius * scale, WHITE)
        drawCircle(earth.coord*scale, equalGravityRadius(earth.currentState, moon.currentState)*scale, color = DARK_GRAY)
        openglLocalTransform {
          openglMove(earth.coord*scale)
          val current_ang = earth.currentState.ang
          (0.0 to 359.0 by 600.0/globalScale).init.map(_.toInt).distinct.foreach {
            case ang =>
              openglLocalTransform {
                openglRotateDeg(current_ang + ang.toDouble)
                openglMove(DVec(0, 1) * (earth.radius - 20000000 / globalScale) * scale)
                print(
                  s"$ang",
                  Vec.zero,
                  color = WHITE,
                  size = (max_font_size / globalScale).toFloat,
                  align = "bottom-center"
                )
              }
          }
        }

        drawCircle(moon.coord*scale, moon.radius * scale, WHITE)
        drawCircle(moon.coord*scale, equalGravityRadius(moon.currentState, earth.currentState)*scale, color = DARK_GRAY)
        drawCircle(moon.coord*scale, soi(moon.mass, earth.coord.dist(moon.coord), earth.mass)*scale, color = DARK_GRAY)
        calculateOrbit(earth.mass, earth.coord, moon.mass, moon.coord, moon.linearVelocity, G) match {
          case moon_orbit:EllipseOrbit =>
            openglLocalTransform {
              openglMove(moon_orbit.center*scale)
              openglRotateDeg(Vec(-1,0).signedDeg(moon_orbit.f2-moon_orbit.f))
              drawEllipse(DVec.zero, moon_orbit.a * scale, moon_orbit.b * scale, GREEN)
            }
            openglLocalTransform {
              openglMove(moon.coord*scale)
              val current_ang = moon.currentState.ang
              (0.0 to 359.0 by 6000.0/globalScale).init.map(_.toInt).distinct.foreach {
                case ang =>
                  openglLocalTransform {
                    openglRotateDeg(current_ang + ang.toDouble)
                    openglMove(DVec(0, 1) * (moon.radius - 20000000 / globalScale) * scale)
                    print(
                      s"$ang",
                      Vec.zero,
                      color = WHITE,
                      size = (max_font_size / globalScale).toFloat,
                      align = "bottom-center"
                    )
                  }
              }
            }
          case _ =>
        }
        drawFilledCircle(ship.coord*scale, earth.radius * scale / 2f / globalScale, WHITE)
        ship_orbit_render()
        // если у корабля активны двигатели
        /*if(ship.engines.exists(_.active)) {
          // получаем состояние системы, когда двигатели отработали
          val system_state_when_engines_off = getFutureState(ships.flatMap(_.engines.map(e => e.worktimeTacts)).max)
          shipOrbitRender(ship.index, system_state_when_engines_off, PURPLE, RED)

        } else {
          // двигатели корабля не работают - можно работать с текущим состоянием
          shipOrbitRender(ship.index, currentSystemState, ORANGE, YELLOW)
          /*val one_week_evolution_after_engines_off = futureSystemEvolutionWithoutReactiveForcesFrom(
            3600 * base_dt, _tacts, currentSystemState, enable_collisions = false)
            .take(7* 24 * 63)
            .zipWithIndex
            .filter(_._2 % 5 == 0)
            .map(_._1)
            .map(_._2)
            .toList
          drawSlidingLines(one_week_evolution_after_engines_off.flatMap(x => x.find(_.index == ship.index)).map(_.coord * scale), ORANGE)*/
        }*/
        /*val radius = ship.linearVelocity.norma2/(ship.linearAcceleration* ship.linearVelocity.p)
        val point = if(radius >= 0) ship.coord + ship.linearVelocity.p*radius else ship.coord - ship.linearVelocity.p*radius
        drawCircle(point*scale, math.abs(radius)*scale, DARK_GRAY)*/

        drawFilledCircle(station.coord*scale, earth.radius * scale / 2f / globalScale, WHITE)
        //shipOrbitRender(station.index, currentSystemState, ORANGE, YELLOW)
        station_orbit_render()

        for {
          x <- left_up_corner
          y <- right_down_corner
        } {
          val c = (y-x).n*(y.dist(x)/2f)+x
          val w = math.abs(y.x - x.x)
          val h = math.abs(y.y - x.y)

          drawRectCentered(c,w,h,DARK_GRAY)
          drawLine(c + DVec(-w/2, -h/2), c + DVec(-w/2, -h/2) + DVec(0, -10/globalScale))
          drawLine(c + DVec(w/2, h/2),   c + DVec(w/2, h/2)   + DVec(0, -10/globalScale))

          drawArrow(c + DVec(0, -h/2) + DVec(0, -5/globalScale), c + DVec(-w/2, -h/2) + DVec(0, -5/globalScale),DARK_GRAY)
          drawArrow(c + DVec(0, -h/2) + DVec(0, -5/globalScale), c + DVec(w/2, -h/2)  + DVec(0, -5/globalScale),DARK_GRAY)

          openglLocalTransform {
            val k = messageBounds(s"${mOrKm((w / scale).toInt)}", (max_font_size / globalScale).toFloat)
            openglMove((c + DVec(0, -h / 2) + DVec(0, -15 / globalScale) + DVec(-k.x / 2, -k.y / 2)).toVec)
            print(
              s"${mOrKm((w / scale).toInt)}",
              Vec.zero,
              color = DARK_GRAY,
              size = (max_font_size / globalScale).toFloat
            )
          }

          drawLine(c + DVec(w/2, h/2),  c + DVec(w/2, h/2)  + DVec(10/globalScale, 0))
          drawLine(c + DVec(w/2, -h/2), c + DVec(w/2, -h/2) + DVec(10/globalScale, 0))

          drawArrow(c + DVec(w/2, 0) + DVec(5/globalScale, 0), c + DVec(w/2, h/2) + DVec(5/globalScale, 0),DARK_GRAY)
          drawArrow(c + DVec(w/2, 0) + DVec(5/globalScale, 0), c + DVec(w/2, -h/2) + DVec(5/globalScale, 0),DARK_GRAY)

          openglLocalTransform {
            val l = messageBounds(s"${mOrKm((h / scale).toInt)}", (max_font_size / globalScale).toFloat)
            openglMove((c + DVec(w / 2, 0) + DVec(10 / globalScale, 0) + DVec(0, -l.y / 2)).toVec)
            print(
              s"${mOrKm((h / scale).toInt)}",
              Vec.zero,
              color = DARK_GRAY,
              size = (max_font_size / globalScale).toFloat
            )
          }
        }
      } else {
        val m = absCoord(mouseCoord)
        val d = ship.coord.dist(m)
        openglLocalTransform {
          openglMove(ship.coord - base)
          drawArrow(DVec.zero, m-ship.coord, DARK_GRAY)
          openglMove(m-ship.coord)
          openglRotateDeg(-rotationAngleDeg)
          print(s"  ${mOrKm(d.toLong)}", Vec.zero, size = (max_font_size / globalScale).toFloat, DARK_GRAY)
        }

      }
    }
  }

  interface {
    if(onPause) print("Пауза", windowCenter.toVec, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$ticks", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec*fps/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%/${1*averageActionTimeMsec*ticks/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%", windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec", windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec", windowWidth - 20, windowHeight - 100, align = "top-right", color = DARK_GRAY)

    if(_rendering_enabled) {
      val a = DVec(windowWidth - 250, 20)
      val b = DVec(windowWidth - 250 + 100, 20)
      drawLine(a,b, DARK_GRAY)
      drawLine(a, a+(a-b).rotateDeg(90).n*5, DARK_GRAY)
      drawLine(b, b+(a-b).rotateDeg(90).n*5, DARK_GRAY)
      print(s"${mOrKm((100/globalScale/(if(drawMapMode) scale else 1.0)).toInt)}", b.toVec, DARK_GRAY)
    }

    if(!disable_interface_drawing) {
      if(_rendering_enabled) {
        openglLocalTransform {
          val z = windowHeight/2f-40f
          openglMove(windowCenter)
          openglRotateDeg(rotationAngleDeg)
          drawArrow(DVec(0, -z), DVec(0, z), DARK_GRAY, 1)
          print("y", Vec(0, z), DARK_GRAY)
          drawArrow(DVec(-z, 0), DVec(z, 0), DARK_GRAY, 1)
          print("x", Vec(z, 0), DARK_GRAY)
        }
      }

      val other_ships_near = ship.otherShipsNear
      val engines_active = ship.engines.exists(_.active)

      val heights = {
        var base_max_height = 420
        if(_stop_after_number_of_tacts > 0)  {
          base_max_height += 20
        }
        if(_show_game_saved_message) {
          base_max_height += 20
        }
        if(_show_game_loaded_message) {
          base_max_height += 20
        }
        if(_show_game_failed_to_load_message) {
          base_max_height += 20
        }
        if(other_ships_near.nonEmpty) {
          base_max_height += 20
        }
        if(engines_active && ship.flightMode != 0) {
          base_max_height += 3*20
        }

        (base_max_height to 20 by -20).iterator
      }

      if(_show_game_saved_message) {
        print(s"Игра сохранена", 20, heights.next(), YELLOW)
      }
      if(_show_game_loaded_message) {
        print(s"Игра загружена", 20, heights.next(), YELLOW)
      }
      if(_show_game_failed_to_load_message) {
        print(s"Не удалось загрузить игру", 20, heights.next(), YELLOW)
      }
      print(s"Время: ${timeStr((_tacts*base_dt*1000).toLong)}", 20, heights.next(), YELLOW)
      // (${if(timeMultiplier < maxTimeMultiplier) 1 else timeMultiplier/factors5(timeMultiplier).filter(_ <= maxTimeMultiplier).max})
      print(f"Ускорение времени: x${(timeMultiplier*k).toInt}/$maxTimeMultiplier (${1f*timeMultiplier/63*ticks}%.2f)",
        20, heights.next(), YELLOW)
      if(_stop_after_number_of_tacts > 0) {
        print(s"Пауза через: ${timeStr((_stop_after_number_of_tacts*base_dt*1000).toLong)}",
          20, heights.next(), YELLOW)
      }

      if(_rendering_enabled) {
        print("", 20, heights.next(), YELLOW)

        print(s"Режим камеры: $viewModeStr",
          20, heights.next(), YELLOW)
        print(s"Полетный режим: ${ship.flightModeStr}",
          20, heights.next(), YELLOW)
        print(ship_states_with_different_max_multipliers.map(x => s"${x._1} ${mOrKm(x._2)}").mkString(" : "), 20, heights.next(), YELLOW)

        print("", 20, heights.next(), YELLOW)

        val ship_above_earth_speed = msecOrKmsec((ship.linearVelocity*(ship.coord - earth.coord).p)/ship.coord.dist(earth.coord)*earth.radius - earth.ground_speed_msec)

        print(f"""Расстояние и скорость относительно Земли: ${mOrKm(ship.coord.dist(earth.coord) - earth.radius)}, ${msecOrKmsec((ship.linearVelocity - earth.linearVelocity) * (ship.coord - earth.coord).n)}, $ship_above_earth_speed""",
          20, heights.next(), YELLOW)
        print(f"Расстояние и скорость относительно Луны: ${mOrKm(ship.coord.dist(moon.coord) - moon.radius)}, ${msecOrKmsec((ship.linearVelocity - moon.linearVelocity)* (ship.coord - moon.coord).n)}",
          20, heights.next(), YELLOW)
        other_ships_near.headOption.foreach {
          case os =>
            print(f"Расстояние и скорость относительно ближайшего корабля: ${mOrKm(ship.coord.dist(os.coord))}, ${msecOrKmsec((ship.linearVelocity - os.linearVelocity)* (ship.coord - os.coord).n)}",
              20, heights.next(), YELLOW)
        }

        print("", 20, heights.next(), YELLOW)

        print(f"Линейная скорость: ${msecOrKmsec(ship.linearVelocity.norma)}",
          20, heights.next(), YELLOW)
        /*print(f"Линейная ускорение: ${msec2OrKmsec2(ship.linearAcceleration.norma)} (tan = ${msec2OrKmsec2(ship.linearAcceleration*ship.linearVelocity.n)}, cen = ${msec2OrKmsec2(math.abs(ship.linearAcceleration*ship.linearVelocity.p))})",
          20, heights.next(), YELLOW)*/
        /*print(f"Радиус кривизны траектории в данной точке: ${curvatureRadiusStrInPoint(ship.currentState)}",
          20, heights.next(), YELLOW)*/
        /*print(f"Угол: ${ship.rotation}%.2f град",
          20, heights.next(), YELLOW)*/
        print(f"Угловая скорость: ${ship.angularVelocity}%.2f град/сек",
          20, heights.next(), YELLOW)
        print(ship.pilotStateStr(75, DVec(0, 69)),
          20, heights.next(), YELLOW)

        print("", 20, heights.next(), YELLOW)

        val earth_force = gravityForce(earth.coord, earth.mass, ship.coord, ship.mass, G).norma
        val moon_force = gravityForce(moon.coord, moon.mass, ship.coord, ship.mass, G).norma
        print(f"Влияние планет: Земля ${earth_force/(earth_force + moon_force)*100}%.2f% Луна ${moon_force/(earth_force + moon_force)*100}%.2f% З/Л ${earth_force/moon_force}%.2f Л/З ${moon_force/earth_force}%.2f",
          20, heights.next(), YELLOW)
        insideSphereOfInfluenceOfCelestialBody(ship.coord, ship.mass, currentPlanetStates) match {
          case Some((planet, planet_state)) =>
            val planet_name = planet.index match {
              case earth.index => "Земля"
              case moon.index => "Луна"
            }
            val sat_speed = satelliteSpeed(ship.coord, ship.linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G).norma
            val esc_speed = escapeVelocity(ship.coord, ship.linearVelocity, planet_state.coord, planet_state.vel, planet_state.mass, G).norma
            print(f"Скорость спутника/убегания: $planet_name ${msecOrKmsec(sat_speed)}/${msecOrKmsec(esc_speed)}",
              20, heights.next(), YELLOW)
          case None =>
        }
        /*val earth_sat_speed = satelliteSpeed(ship.coord, ship.linearVelocity, earth.coord, earth.linearVelocity, earth.mass, G).norma
        val earth_esc_speed = escapeVelocity(ship.coord, ship.linearVelocity, earth.coord, earth.linearVelocity, earth.mass, G).norma
        val moon_sat_speed = satelliteSpeed(ship.coord, ship.linearVelocity, moon.coord, moon.linearVelocity, moon.mass, G).norma
        val moon_esc_speed = escapeVelocity(ship.coord, ship.linearVelocity, moon.coord, moon.linearVelocity, moon.mass, G).norma

        val ship_earth_orbital_energy = specificOrbitalEnergy(earth.mass, earth.coord, ship.mass, ship.coord - earth.coord, ship.linearVelocity-earth.linearVelocity, G)
        val ship_moon_orbital_energy = specificOrbitalEnergy(moon.mass, moon.coord, ship.mass, ship.coord - moon.coord, ship.linearVelocity-moon.linearVelocity, G)*/
        /*print(f"Орбитальная энергия: Земля $ship_earth_orbital_energy%.2f Луна $ship_moon_orbital_energy%.2f",
          20, heights.next(), YELLOW)*/
        print(s"Параметры орбиты: ${orbitStrInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass, currentPlanetStates)}",
          20, heights.next(), YELLOW)

        print("", 20, heights.next(), YELLOW)

        print(s"Двигательная установка: ${if(engines_active) "[rактивирована]" else "отключена"}",
          20, heights.next(), YELLOW)

          print(s"Мощность и время работы отдельных двигателей:",
            20, heights.next(), YELLOW)
          print(s"${
            ship.engines.map(e => {
              if (e.active) {
                if (ship.isSelectedEngine(e)) s"[r${e.powerPercent} % (${e.workTimeStr})]"
                else s"[o${e.powerPercent} % (${e.workTimeStr})]"
              } else s"${e.powerPercent} % (${e.workTimeStr})"
            }).mkString(", ")
          }",
            20, heights.next()
            , YELLOW)
        if(engines_active && ship.flightMode != 0) {
          print(f"Линейная скорость в момент отключения двигателей: $linearSpeedStrWhenEnginesOff",
            20, heights.next(), YELLOW)
          print(f"Угловая скорость в момент отключения двигателей: $angularSpeedStrWhenEnginesOff",
            20, heights.next(), YELLOW)
          print(s"Параметры орбиты в момент отключения двигателей: $orbitParametersStrWhenEnginesOff",
            20, heights.next(), YELLOW)
        }
      }
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
  def currentState:BodyState
  def initState:BodyState
}

class Planet(
  val index:String,
  val mass:Double,
  val init_coord:DVec,
  val init_velocity:DVec,
  val init_ang_vel:Double,
  val radius:Double) extends CelestialBody {

  def coord = currentState.coord
  def linearVelocity = currentState.vel

  def initState:BodyState = BodyState(
      index,
      mass,
      acc = DVec.dzero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0,
      ang_vel = init_ang_vel,
      ang = 0,
      shape = CircleShape(radius),
      is_static = false)

  def currentState:BodyState = currentBodyState(index).getOrElse(initState)

  render {
    if(renderingEnabled) {
      if(!drawMapMode) {
        val viewpoint_dist = math.abs((ship.coord + shipOffset).dist(coord) - radius)
        if (viewpoint_dist < 50000) {
          openglLocalTransform {
            openglMove(coord - base)
            val to_viewpoint = (ship.coord + shipOffset - coord).n * radius
            val alpha = 100000 * 180 / math.Pi / radius
            val points = for {
              ang <- -alpha to alpha by 0.01
              point = to_viewpoint.rotateDeg(ang)
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
  }
}

class Star(val index:String, val mass:Double, val coord:DVec, ang_vel:Double, val radius:Double) extends CelestialBody {
  def initState:BodyState = BodyState(
      index = index,
      mass = mass,
      acc = DVec.dzero,
      vel = DVec.dzero,
      coord = coord,
      ang_acc = 0,
      ang_vel = ang_vel,
      ang = 0,
      shape = CircleShape(radius),
      is_static = false)

  def currentState:BodyState = currentBodyState(index).getOrElse(initState)

  val ground_length_km = (2*math.Pi*radius/1000).toInt
  val ground_speed_msec = 2*math.Pi*radius/(24*60*60)

  // рельеф планеты: треугольные горы. два параметра: высота в метрах, ширина основания в метрах
  private val ground_features = Array.ofDim[(Int, Int)](ground_length_km)
  (0 until ground_length_km).foreach(x => ground_features(x) = (100 + (math.random*900).toInt, 50 + (math.random*300).toInt))

  private def groundFeatureNear(point_km:Double):(Int, Int) = {
    if(point_km < 0) groundFeatureNear(point_km + ground_length_km)
    else if(point_km >= ground_length_km) groundFeatureNear(point_km - ground_length_km)
    else ground_features(point_km.toInt)
  }

  render {
    if(renderingEnabled) {
      if(!drawMapMode) {
        val viewpoint_dist = math.abs((ship.coord + shipOffset).dist(coord) - radius)
        if (viewpoint_dist < 50000) {
          openglLocalTransform {
            openglMove(coord - base)
            val to_viewpoint = (ship.coord + shipOffset - coord).n * radius
            val alpha = 100000 * 180 / math.Pi / radius // угловой размер для 100 километров поверхности
            val points = for {
              ang <- -alpha to alpha by 0.01
              point = to_viewpoint.rotateDeg(ang)
            } yield point
            drawSlidingLines(points, WHITE)

            val ground_position_ang = correctAngle(to_viewpoint.mydeg(Vec(0, 1)) - currentState.ang)
            val ground_position_km = ground_position_ang / 360.0 * 2 * math.Pi * radius / 1000
            println(ground_position_km)
            for {
              real_point <- ground_position_km - 50.0 to ground_position_km + 49.0 by 1.0
              (w, h) = groundFeatureNear(real_point)
              point_ang = (360.0*real_point.toInt/ground_length_km) - ground_position_ang
              p = to_viewpoint.rotateDeg(point_ang)
            } {
              drawLine(p + p.p*w/2, p + p.n*h, WHITE)
              drawLine(p - p.p*w/2, p + p.n*h, WHITE)
              drawLine(p, p + p.n*h, WHITE)
            }
          }
        }
      }
    }
  }

  def linearVelocity: DVec = DVec.dzero
}






