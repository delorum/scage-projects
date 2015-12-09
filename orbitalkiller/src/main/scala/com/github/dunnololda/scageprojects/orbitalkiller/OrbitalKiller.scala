package com.github.dunnololda.scageprojects.orbitalkiller

import java.io.FileOutputStream

import com.github.dunnololda.scage.ScageLibD._

import scala.collection.mutable.ArrayBuffer

//import collection.mutable.ArrayBuffer
import scala.collection.mutable

// TODO: implement this
sealed trait ViewMode
sealed trait FlightMode

object OrbitalKiller extends ScageScreenAppDMT("Orbital Killer", property("screen.width", 1280), property("screen.height", 768)) {
  val k:Double = 1 // доля секунды симуляции, которая обрабатывается за одну реальную секунду, если не применяется ускорение

  // движок делает вызов обработчика примерно 63 раза в секунду, за каждый вызов будет обрабатывать вот такую порцию симуляции
  // то есть, мы хотим, чтобы за одну реальную секунду обрабатывалось k секунд симуляции, поэтому за один такт движка (которых 60 в секунду)
  // будем обрабатывать k/60
  val base_dt:Double = 1.0/63*k

  // какой длины в пикселях на экране будет реальная длина в 1 метр
  /*val zoom:Double = 10*/

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

  /*private var _tacts = 0l
  def tacts:Long = _tacts*/

  val system_evolution = new SystemEvolution(base_dt)
  def tacts:Long = system_evolution.tacts

  /*val current_body_states = mutable.HashMap[String, MutableBodyState]()
  def currentBodyState(index:String):Option[MutableBodyState] = current_body_states.get(index)*/
  //def currentSystemState = current_body_states.values.toList.map(_.toImmutableBodyState)
  def currentBodyState(index:String):Option[MutableBodyState] = system_evolution.bodyState(index)

  /*def futureSystemEvolutionFrom(dt: => Double, tacts:Long, body_states:List[BodyState], enable_collisions:Boolean) = systemEvolutionFrom(
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
        case bs =>
          // зануляем угловую скорость, если она меньше 0.01. Обновляем массу
          bs.index match {
            case ship.index =>
              bs.copy(
                ang_vel = if(bs.ang_vel != 0 && math.abs(bs.ang_vel) < 0.01) 0 else bs.ang_vel,
                mass = ship.currentMass(time, bs)
              )
            case _ => bs
          }
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
    enable_collisions = enable_collisions)((tacts, body_states))*/

  //private var continue_future_trajectory = false
  //val trajectory_accuracy = 100 // для рисования: рисуем только каждую сотую точку траектории
  //val trajectory_capacity = 100000

  /*private val future_trajectory = ArrayBuffer[(Long, List[BodyState])]()
  def futureTrajectory:Seq[(Long, List[BodyState])] = future_trajectory*/
  //private val future_trajectory_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()
  //private val future_trajectory_capacity = 10000

  /*private val system_cache = mutable.HashMap[Long, mutable.Map[String, MutableBodyState]]()

  def getFutureState(tacts:Long):mutable.Map[String, MutableBodyState] = {
    if(ship.flightMode != 0) {
      /*if (future_trajectory.length >= tacts) future_trajectory(tacts.toInt)._2
      else {
        continueFutureTrajectory(s"getFutureState($tacts)")
        getFutureState(tacts)
      }*/
      system_cache.getOrElseUpdate(tacts, {
        println("adding to system_cache")
        val system_copy = makeThisAndOthers(our_mutable_system.map(_._1.copy))
        val steps = (tacts - _tacts).toInt
        (1 to steps).foreach(step => ourMutableSystemEvolution(system_copy))
        mutable.Map(system_copy.map(x => (x._1.index, x._1)):_*)
      })
    } else collection.mutable.Map()
  }*/

  //private val body_trajectories_map = mutable.HashMap[String, ArrayBuffer[(Long, BodyState)]]()

  def updateFutureTrajectory(reason:String) {
    if(ship.flightMode != 0) {
      println(s"updateFutureTrajectory: $reason")
      //system_cache.clear()
      /*future_trajectory.clear()
      future_trajectory ++= {
        futureSystemEvolutionFrom(base_dt, _tacts, currentSystemState, enable_collisions = false)
          .take(future_trajectory_capacity)
      }*/
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

  /*def continueFutureTrajectory(reason:String) {
    if(ship.flightMode != 0) {
      println(s"continueFutureTrajectory: $reason")
      /*val (t, s) = future_trajectory.lastOption.getOrElse((_tacts, currentSystemState))
      val steps = {
        futureSystemEvolutionFrom(base_dt, t, s, enable_collisions = false)
          .take(future_trajectory_capacity)
      }.toSeq
      future_trajectory ++= steps*/
      //continueFutureTrajectoryMap(steps)
    }
  }
*/
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

  val earth = new Planet(
    "Earth", "Земля",
    mass = 5.9746E24,
    init_coord = DVec.dzero,
    init_velocity = DVec.zero,
    //init_ang_vel = 0.0,
    init_ang_vel = 360.0/(24l*60*60),
    radius = 6400000/*6314759.95726045*/) {

    val T0 = 288     // temperature at sea level, K
    //val L = 0.0065   // temperature lapse rate, K/m
    val L = 0.00288  // temperature lapse rate, K/m
    val P0 = 101325  // pressure at sea level, N/m^2
    val M = 0.02896  // molar mass or air, kg/mole
    val R = 8.314    // ideal gas constant, Joules/(K*mole)

    def altitude(ship_coord:DVec, planet_coord:DVec):Double = ship_coord.dist(planet_coord) - radius
    
    def velocityRelativeToAir(ship_coord:DVec, ship_velocity:DVec, planet_coord:DVec, planet_velocity:DVec, planet_ang_vel:Double):DVec = {
      (ship_velocity - planet_velocity) - (ship_coord - planet_coord).p*(planet_ang_vel.toRad*ship_coord.dist(planet_coord))
    }
    
    def temperature(h:Double) = {
      T0 - L*h
    }

    def airPressurePascale(h:Double):Double = {
      val T = temperature(h)
      if(T <= 0) 0
      else {
        P0 * math.pow(T / T0, g * M / (R * L))
      }
    }

    def airPressureMmHg(h:Double):Double = {
      airPressurePascale(h)/133.3
    }

    // A - Reference area (of the front of the ship)
    // C - Drag coefficient
    def airResistance(v:DVec, h:Double, A:Double, C:Double):DVec = {
      val T = temperature(h)
      if(T <= 0) DVec.zero
      else {
        val P = airPressurePascale(h)
        val ro = P*M/(R*T)                       // density of air
        val F = 0.5*ro*A*C*v.norma2
        -v.n*F
      }
    }

    def airResistance(ship_coord:DVec, ship_velocity:DVec, planet_coord:DVec, planet_velocity:DVec, planet_ang_vel:Double, A:Double, C:Double):DVec = {
      airResistance(velocityRelativeToAir(ship_coord, ship_velocity, planet_coord, planet_velocity, planet_ang_vel), altitude(ship_coord, planet_coord), A, C)
    }

    def airResistance(ship_mutable_state:MutableBodyState, planet_mutable_state:MutableBodyState, A:Double, C:Double):DVec = {
      airResistance(ship_mutable_state.coord, ship_mutable_state.vel, planet_mutable_state.coord, planet_mutable_state.vel, planet_mutable_state.ang_vel, A, C)
    }

    def airPressureMmHg(ship_coord:DVec, planet_coord:DVec):Double = {
      airPressureMmHg(altitude(ship_coord, planet_coord))
    }

    // another model
    // http://fiz.1september.ru/articlef.php?ID=200801702

    /*def airResistance(v:DVec, h:Double):DVec = {
      val c = 0.045                   // безразмерный коэффициент (равный 0,045 для «каплевидного» тела)
      val ro0 = 1.22                  // плотность воздуха на поверхности Земли, кг/м^3
      val beta = 5.6E-5               // м^-1
      val S = math.Pi*3*3             // площадь поперечного сечения тела
      val ro = ro0*math.exp(-beta*h)  // плотность воздуха на высоте h
      val k = c*ro*S                  // коэффициент лобового сопротивления
      val F = k*v.norma2
      //val F = 0.045*v.norma2*1.225*math.exp(-5.6E-5*h)*math.Pi*3*3
      //val F = 0.045*v.norma2*1.225*math.exp(-1.4E-4*h)*math.Pi*3*3
      -v.n*F
    }*/

    /*def airPressurePascale(h:Double):Double = {
      101325*math.exp(-1.1855831477936685E-4*h)
    }*/
  }

  val moon_start_position = DVec(-269000000, 269000000)
  val moon_init_velocity = satelliteSpeed(moon_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)
  val moon = new Planet(
    "Moon", "Луна",
    mass = 7.3477E22,
    init_coord = moon_start_position,
    init_velocity = moon_init_velocity,
    init_ang_vel = 360.0/(26l*24*60*60 + 8l*60*60 + 59l*60 + 44),   // период орбиты луны в данной симуляции: 26 д. 8 ч. 59 мин. 44 сек, равен периоду обращения вокруг собственной оси
    radius = 1737000)

  //val ship_start_position = earth.coord + DVec(500, earth.radius + 3.5)
  //val ship_init_velocity = earth.linearVelocity + (ship_start_position - earth.coord).p*earth.groundSpeedMsec/*DVec.zero*/

  //val ship_start_position = earth.coord + DVec(100, earth.radius + 200000)
  //val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)/**1.15*/

  val ship_start_position = moon.coord + DVec(500, moon.radius + 3.5)
  val ship_init_velocity = moon.linearVelocity + (ship_start_position - moon.coord).p*moon.groundSpeedMsec/*DVec.zero*//*satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.15*/
  //val ship_init_velocity = -escapeVelocity(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.01

  //val ship_start_position = moon.coord + DVec(0, moon.radius + 3000)
  //val ship_init_velocity = satelliteSpeed(ship_start_position, moon.coord, moon.linearVelocity, moon.mass, G, counterclockwise = true)*1.05
  //val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.15

  val ship = new Ship4("ship",
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 0
  )

  val station_start_position = earth.coord + DVec(0, earth.radius + 200000)
  val station_init_velocity = satelliteSpeed(station_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)
  val station = new SpaceStation2("station",
    init_coord = station_start_position,
    init_velocity = station_init_velocity,
    init_rotation = 45
  )

  /*def makeThisAndOthers[A](s:mutable.Buffer[A]):mutable.Buffer[(A, mutable.Buffer[A])] = {
    s.zipWithIndex.map {
      case (b, idx) =>
        (b, s.take(idx) ++ s.drop(idx+1))
    }
  }*/

  /*val our_mutable_system = makeThisAndOthers(ArrayBuffer[MutableBodyState](
    ship.currentState,
    station.currentState,
    moon.currentState,
    earth.currentState))

  our_mutable_system.foreach(x => current_body_states(x._1.index) = x._1)

  def ourMutableSystemEvolution(system:Seq[(MutableBodyState, Seq[MutableBodyState])]) {
    mutableSystemEvolution(
      system, base_dt,
      force = (bs, other_bodies) => {
        bs.index match {
          case ship.index =>
            gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
            other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero) +
            ship.currentReactiveForce(tacts, bs) +
            earth.airResistance(bs.coord, bs.vel, 28, 0.5)
          case station.index =>
            gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
              other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero) +
              station.currentReactiveForce(tacts, bs)
          case moon.index =>
            gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G)
          case _ => DVec.dzero
        }
      },
      torque = (bs, other_bodies) => {
        bs.index match {
          case ship.index =>
            ship.currentTorque(tacts)
          case _ =>
            0
        }
      },
      enable_collisions = true)
  }*/

  system_evolution.addBody(
    ship.currentState,
    (tacts, helper) => {
      helper.gravityForceHelper(earth.index, ship.index) +
      helper.gravityForceHelper(moon.index, ship.index) +
      helper.funcOrDVecZero(ship.index, bs => ship.currentReactiveForce(tacts, bs)) +
      helper.funcOfArrayOrDVecZero(Array(ship.index, earth.index), l => {
        val bs = l(0)
        val e = l(1)
        earth.airResistance(bs, e, 28, 0.5)
      })
    },
    (tacts, helper) => {
      helper.funcOrDoubleZero(ship.index, bs => ship.currentTorque(tacts))
    }
  )
  system_evolution.addBody(
    station.currentState,
    (tacts, helper) => {
      helper.gravityForceHelper(earth.index, station.index) +
      helper.gravityForceHelper(moon.index, station.index) +
      helper.funcOrDVecZero(station.index, bs => station.currentReactiveForce(tacts, bs)) +
      helper.funcOfArrayOrDVecZero(Array(station.index, earth.index), l => {
        val bs = l(0)
        val e = l(1)
        earth.airResistance(bs, e, 28, 0.5)
      })
    },
    (tacts, helper) => {
      helper.funcOrDoubleZero(station.index, bs => station.currentTorque(tacts))
    }
  )
  system_evolution.addBody(
    moon.currentState,
    (tacts, helper) => {
      helper.gravityForceHelper(earth.index, moon.index)
    },
    (tacts, helper) => {
      0.0
    }
  )
  system_evolution.addBody(
    earth.currentState,
    (tacts, helper) => {
      DVec.zero
    },
    (tacts, helper) => {
      0.0
    }
  )

  val ships = List(ship, station)
  val ship_indexes = ships.map(_.index).toSet
  def shipByIndex(index:String):Option[Ship] = ships.find(_.index == index)

  val planets = List(earth, moon)
  val planet_indexes = planets.map(_.index).toSet
  val currentPlanetStates = system_evolution.bodyStates(planet_indexes)
  def planetByIndex(index:String):Option[CelestialBody] = planets.find(_.index == index)

  /*private var real_system_evolution =
    futureSystemEvolutionFrom(dt, 0, List(
        ship.currentState,
        station.currentState,
        moon.currentState,
        earth.currentState),
      enable_collisions = true).iterator*/

  var _stop_after_number_of_tacts:Long = 0
  var _stop_in_orbit_true_anomaly:Double = 0

  //private var skipped_points = 0
  /*private var ship_states_with_different_max_multipliers:List[(Int, Double)] = Nil
  private val xx = 250
  private val fos = new FileOutputStream(s"errors_stats_$xx.txt", true)
  clear {
    fos.close()
  }*/

  private def nextStep() {
    val steps = math.max((dt/base_dt).toInt, 1)
    (1 to steps).foreach(step => {
      ships.foreach(s => s.engines.foreach(e => {
        if(e.active) {
          if(e.workTimeTacts <= 0 || ship.fuelMass <= 0) {
            e.active = false
          } else {
            if(e.ship.fuelMass - e.fuelConsumptionPerTact <= 0) {
              e.active = false
            }
          }
        }
      }))
      if(ship.currentState.ang_vel != 0 && math.abs(ship.currentState.ang_vel) < 0.01) {
        ship.currentState.ang_vel = 0
      }
      ship.currentState.mass = ship.mass/*currentMass(_tacts)*/
      //ourMutableSystemEvolution(our_mutable_system)
      system_evolution.step()
      if(_stop_after_number_of_tacts > 0) {
        _stop_after_number_of_tacts -= 1
        if (_stop_after_number_of_tacts <= 0) {
          if (timeMultiplier != realtime) {
            timeMultiplier = realtime
          }
          pause()
        }
      }
      //_tacts += 1
      ship.updateShipState((tacts*base_dt*1000).toLong)
      ships.foreach(s => s.engines.foreach(e => {
        if(e.active) {
          if(e.workTimeTacts > 0) {
            e.workTimeTacts -= 1
            e.ship.fuelMass -= e.fuelConsumptionPerTact
          } else e.active = false
        }
      }))
    })
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
          center = orbitAroundCelestialInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass).map(_._2.center * scale).getOrElse(ship.coord)
          rotationAngle = 0
          view_mode = 4
        }
      case _ =>
    }
  }
  def viewModeStr = view_mode match {
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

  /*def orbitStrInPointWithVelocity_imm(coord:DVec, velocity:DVec, mass:Double, planet_states:Seq[BodyState]):String = {
    insideSphereOfInfluenceOfCelestialBody_imm(coord, mass, planet_states) match {
      case Some((planet, planet_state)) =>
        val prefix = planet.index match {
          case earth.index => "Земля"
          case moon.index => "Луна"
        }
        val orbit = calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)
        orbit.strDefinition(prefix, planetByIndex(planet_state.index).get.radius, planet_state.vel, coord, velocity)
      case None => "N/A"
    }
  }*/

  def orbitStrInPointWithVelocity(coord:DVec, velocity:DVec, mass:Double, planet_states:mutable.Map[String, MutableBodyState]):String = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, planet_states) match {
      case Some((planet, planet_state)) =>
        val orbit = calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)
        orbit.strDefinition(planet.name, planetByIndex(planet_state.index).get.radius, planet_state.vel, planet.g, coord, velocity)
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

  /*def orbitAroundCelestialInPointWithVelocity(coord:DVec, velocity:DVec, mass:Double, planet_states:Seq[BodyState]):Option[((CelestialBody, BodyState), KeplerOrbit)] = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        planetByIndex(planet_state.index).flatMap(planet => {
          Some(((planet, planet_state), calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)))
        })
      case None => None
    }
  }*/

  def orbitAroundCelestialInPointWithVelocity(coord:DVec, velocity:DVec, mass:Double):Option[((CelestialBody, MutableBodyState), KeplerOrbit)] = {
    insideSphereOfInfluenceOfCelestialBody(coord, mass, currentPlanetStates) match {
      case Some((planet, planet_state)) =>
        planetByIndex(planet_state.index).flatMap(planet => {
          Some(((planet, planet_state), calculateOrbit(planet_state.mass, planet_state.coord, mass, coord - planet_state.coord, velocity - planet_state.vel, G)))
        })
      case None => None
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
      orbitAroundCelestialInPointWithVelocity(ship.coord, ship.linearVelocity, ship.mass) match {
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
              val aabb = b.aabb(hyperbola.half_center, Vec(1,0).signedDeg(hyperbola.f_minus_center_n))
              viewMode = 0
              globalScale = 750 / (aabb.height * scale)
              _center = hyperbola.half_center*scale
          }
        case None =>
          viewMode = 0
          globalScale = 1
          _center = earth.coord*scale
      }
    } else {
      _draw_map_mode = false
      globalScale = 10
      viewMode = 1
    }
  }

  /*def insideSphereOfInfluenceOfCelestialBody_imm(ship_coord:DVec, ship_mass:Double, planet_states:Seq[BodyState]):Option[(CelestialBody, BodyState)] = {
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
  }*/

  def insideSphereOfInfluenceOfCelestialBody(ship_coord:DVec, ship_mass:Double, planet_states:collection.mutable.Map[String, MutableBodyState]):Option[(CelestialBody, MutableBodyState)] = {
    for {
      moon_state <- planet_states.get(moon.index)
      earth_state <- planet_states.get(earth.index)
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

  preinit {
    addGlyphs("\u21b6\u21b7")
  }

  keyIgnorePause(KEY_NUMPAD1, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD1)})
  keyIgnorePause(KEY_NUMPAD2, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD2)})
  keyIgnorePause(KEY_NUMPAD3, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD3)})
  keyIgnorePause(KEY_NUMPAD4, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD4)})
  keyIgnorePause(KEY_NUMPAD6, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD6)})
  keyIgnorePause(KEY_NUMPAD7, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD7)})
  keyIgnorePause(KEY_NUMPAD8, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD8)})
  keyIgnorePause(KEY_NUMPAD9, onKeyDown = {if(ship.pilotIsAlive) ship.selectOrSwitchEngineActive(KEY_NUMPAD9)})

  keyIgnorePause(KEY_NUMPAD5, onKeyDown = {
    if(ship.pilotIsAlive) {
      ship.engines.foreach(e => {
        if (e.active || e.power > 0) {
          e.active = false
          e.power = 0
        } else {
          e.workTimeTacts = 0
        }
      })
      ship.selected_engine = None
    }
  })

  /*keyIgnorePause(KEY_NUMPAD0, onKeyDown = {
    ship.engines.filterNot(_.active).foreach(e => e.workTimeTacts = 226800)     // 1 hour
    val active_engines = ship.engines.filter(_.active)
    if(active_engines.map(ae => ae.fuelConsumptionPerTact*226800).sum <= ship.fuelMass) {
      active_engines.foreach(e => e.workTimeTacts = 226800)
    } else {
      val fuel_for_every_active_engine = ship.fuelMass / active_engines.length
      active_engines.foreach(e => e.workTimeTacts = (fuel_for_every_active_engine/e.fuelConsumptionPerTact).toLong)
    }
  })*/


  private def repeatTime(code:Int):Long = {
    keyPress(code).map {
      case kp =>
        if(kp.was_pressed && System.currentTimeMillis() - kp.pressed_start_time < 100) 100l
        else 10l
    }.getOrElse(100l)
  }

  keyIgnorePause(KEY_UP,      repeatTime(KEY_UP),    onKeyDown = {
    if(ship.pilotIsAlive) {
      if (ship.flightMode != 8) {
        ship.selected_engine.foreach(e => e.powerPercent += 1)
      } else {
        ship.vertical_speed_msec += 1
      }
    }
  }, onKeyUp = if(ship.pilotIsAlive && ship.flightMode != 8) updateFutureTrajectory("KEY_UP"))
  keyIgnorePause(KEY_DOWN,    repeatTime(KEY_DOWN),  onKeyDown = {
    if(ship.pilotIsAlive) {
      if (ship.flightMode != 8) {
        ship.selected_engine.foreach(e => e.powerPercent -= 1)
      } else {
        ship.vertical_speed_msec -= 1
      }
    }
  }, onKeyUp = if(ship.pilotIsAlive && ship.flightMode != 8) updateFutureTrajectory("KEY_DOWN"))
  keyIgnorePause(KEY_RIGHT,   repeatTime(KEY_RIGHT), onKeyDown = {
    if(ship.pilotIsAlive) {
      if (ship.flightMode != 8) {
        ship.selected_engine.foreach(e => {
          e.workTimeTacts += 1
          //updateFutureTrajectory()
        })
      } else {
        ship.horizontal_speed_msec -= 1
      }
    }
  }, onKeyUp = if(ship.pilotIsAlive && ship.flightMode != 8) updateFutureTrajectory("KEY_RIGHT"))
  keyIgnorePause(KEY_LEFT,    repeatTime(KEY_LEFT),  onKeyDown = {
    if(ship.pilotIsAlive) {
      if (ship.flightMode != 8) {
        ship.selected_engine.foreach(e => {
          /*if(e.worktimeTacts > 0) {*/
          e.workTimeTacts -= 1
          //updateFutureTrajectory()
          /*}*/
        })
      } else {
        ship.horizontal_speed_msec += 1
      }
    }
  }, onKeyUp = {
    if(ship.pilotIsAlive && ship.flightMode != 8) updateFutureTrajectory("KEY_LEFT")
  })

  private val engine_keys = List(KEY_UP, KEY_DOWN, KEY_RIGHT, KEY_LEFT)
  def anyEngineKeyPressed = engine_keys.exists(k => keyPressed(k))

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

  keyIgnorePause(KEY_1, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 1)
  keyIgnorePause(KEY_2, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 2)
  keyIgnorePause(KEY_3, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 3)
  keyIgnorePause(KEY_4, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 4)
  keyIgnorePause(KEY_5, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 5)
  keyIgnorePause(KEY_6, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 6)
  keyIgnorePause(KEY_7, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 7)
  keyIgnorePause(KEY_8, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 8)
  keyIgnorePause(KEY_9, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 9)
  keyIgnorePause(KEY_0, onKeyDown = if(ship.pilotIsAlive) ship.flightMode = 0)

  keyIgnorePause(KEY_P, onKeyDown = switchPause())

  keyIgnorePause(KEY_F1, onKeyDown = {pause(); holdCounters {HelpScreen.run()}})
  keyIgnorePause(KEY_F2, onKeyDown = if(!drawMapMode) viewMode = 1 else viewMode = 0)     // фиксация на корабле, в режиме карты: свободный режим
  keyIgnorePause(KEY_F3, onKeyDown = if(!drawMapMode) viewMode = 3 else viewMode = 4)     // фиксация на корабле, абсолютная ориентация, в режиме карты: фиксация на орбите
  keyIgnorePause(KEY_F4, onKeyDown = if(!drawMapMode) viewMode = 2)                       // посадка на планету, если не в режиме карты

  keyIgnorePause(KEY_F5, onKeyDown = saveGame())                          // сохранить текущее состояние системы
  keyIgnorePause(KEY_F6, onKeyDown = loadGame())                          // загрузить из файла состояние системы

  private var _show_game_saved_message = false
  def showGameSavedMessage = _show_game_saved_message

  def saveGame() {
    val fos = new FileOutputStream("save.orbitalkiller")
    fos.write(s"time $tacts\n".getBytes)
    currentBodyState(ship.index).foreach {
      case x =>
        fos.write(s"${x.index} ${x.acc.x}:${x.acc.y} ${x.vel.x}:${x.vel.y} ${x.coord.x}:${x.coord.y} ${x.ang_acc} ${x.ang_vel} ${x.ang}\n".getBytes)
    }
    currentBodyState(station.index).foreach {
      case x =>
        fos.write(s"${x.index} ${x.acc.x}:${x.acc.y} ${x.vel.x}:${x.vel.y} ${x.coord.x}:${x.coord.y} ${x.ang_acc} ${x.ang_vel} ${x.ang}\n".getBytes)
    }
    currentBodyState(moon.index).foreach {
      case x =>
        fos.write(s"${x.index} ${x.acc.x}:${x.acc.y} ${x.vel.x}:${x.vel.y} ${x.coord.x}:${x.coord.y} ${x.ang_acc} ${x.ang_vel} ${x.ang}\n".getBytes)
    }
    currentBodyState(earth.index).foreach {
      case x =>
        fos.write(s"${x.index} ${x.acc.x}:${x.acc.y} ${x.vel.x}:${x.vel.y} ${x.coord.x}:${x.coord.y} ${x.ang_acc} ${x.ang_vel} ${x.ang}\n".getBytes)
    }
    fos.close()
    _show_game_saved_message = true
    val start = System.currentTimeMillis()
    actionStaticPeriodIgnorePause(1000) {
      if(System.currentTimeMillis() - start > 2000) {
        _show_game_saved_message = false
        deleteSelf()
      }
    }
  }

  private var _show_game_loaded_message = false
  def showGameLoadedMessage = _show_game_loaded_message

  private var _show_game_failed_to_load_message = false
  def showGameFailedToLoadMessage = _show_game_failed_to_load_message

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
      system_evolution.tacts = new_tacts
      /*real_system_evolution = futureSystemEvolutionFrom(dt, _tacts, List(
          ship.currentState.copy(acc = ship_acc, vel = ship_vel, coord = ship_coord, ang_acc = ship_ang_acc, ang_vel = ship_ang_vel, ang = ship_ang),
          station.currentState.copy(acc = station_acc, vel = station_vel, coord = station_coord, ang_acc = station_ang_acc, ang_vel = station_ang_vel, ang = station_ang),
          moon.currentState.copy(acc = moon_acc, vel = moon_vel, coord = moon_coord),
          earth.currentState),
          enable_collisions = true).iterator*/
      ship.currentState.acc = ship_acc
      ship.currentState.vel = ship_vel
      ship.currentState.coord = ship_coord
      ship.currentState.ang_acc = ship_ang_acc
      ship.currentState.ang_vel = ship_ang_vel
      ship.currentState.ang = ship_ang

      station.currentState.acc = station_acc
      station.currentState.vel = station_vel
      station.currentState.coord = station_coord
      station.currentState.ang_acc = station_ang_acc
      station.currentState.ang_vel = station_ang_vel
      station.currentState.ang = station_ang

      moon.currentState.acc = moon_acc
      moon.currentState.vel = moon_vel
      moon.currentState.coord = moon_coord
      /*moon.currentState.ang_acc = moon_ang_acc
      moon.currentState.ang_vel = moon_ang_vel
      moon.currentState.ang = moon_ang*/
      _show_game_loaded_message = true
    }
    if(_show_game_loaded_message) {
      val start = System.currentTimeMillis()
      actionStaticPeriodIgnorePause(1000) {
        if(System.currentTimeMillis() - start > 2000) {
          _show_game_loaded_message = false
          deleteSelf()
        }
      }
    } else {
      _show_game_failed_to_load_message = true
      val start = System.currentTimeMillis()
      actionStaticPeriodIgnorePause(1000) {
        if(System.currentTimeMillis() - start > 2000) {
          _show_game_failed_to_load_message = false
          deleteSelf()
        }
      }
    }
  }

  /*keyIgnorePause(KEY_Z, onKeyDown = disable_trajectory_drawing = !disable_trajectory_drawing)
  keyIgnorePause(KEY_X, onKeyDown = disable_future_trajectory_drawing = !disable_future_trajectory_drawing)*/
  keyIgnorePause(KEY_I, onKeyDown = {
    disable_interface_drawing = !disable_interface_drawing
    if(disable_interface_drawing) InterfaceHolder.hideAllByUser()
    else InterfaceHolder.showAllByUser()
  })
  //keyIgnorePause(KEY_R, onKeyDown = _rendering_enabled = !_rendering_enabled)
  //keyIgnorePause(KEY_N, onKeyDown = continue_future_trajectory = !continue_future_trajectory)
  /*keyIgnorePause(KEY_C, onKeyDown = {
    continue_future_trajectory = false
    /*updateFutureTrajectory()*/
  })*/

  keyIgnorePause(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  mouseWheelDownIgnorePause(onWheelDown = m => {
    if(globalScale > 0.01) {
      if(globalScale.toInt >= 200000) globalScale -= 100000
      else if(globalScale.toInt >= 20000) globalScale -= 10000
      else if(globalScale.toInt >= 2000) globalScale -= 1000
      else if(globalScale.toInt >= 200) globalScale -= 100
      else if(globalScale.toInt >= 20) globalScale -= 10
      else if(globalScale.toInt >= 2) globalScale -= 1
      else if((globalScale*10).toInt > 1) globalScale -= 0.1
      else globalScale -= 0.01
      if(globalScale < 0.01) globalScale = 0.01
    }
    println(globalScale)
  })
  mouseWheelUpIgnorePause(onWheelUp = m => {
    val _maxGlobalScale = if(!drawMapMode) 30 else 1000000
    if(globalScale < _maxGlobalScale) {
      if(globalScale < 0.1) globalScale += 0.01
      else if(globalScale < 1) globalScale += 0.1
      else if(globalScale < 10) globalScale +=1
      else if(globalScale < 100) globalScale +=10
      else if(globalScale < 1000) globalScale +=100
      else if(globalScale < 10000) globalScale +=1000
      else if(globalScale < 100000) globalScale +=10000
      else globalScale += 100000
      if(globalScale > _maxGlobalScale) globalScale = _maxGlobalScale
    }
    println(globalScale)
  })


  private var left_up_corner:Option[DVec] = None
  private var right_down_corner:Option[DVec] = None
  private var set_stop_moment = false
  leftMouseIgnorePause(onBtnDown = m => {
    if(drawMapMode) {
      left_up_corner = Some(absCoord(m))
    } else {
      InterfaceHolder.determineInterfaceElem(m).foreach(i => if(i.isMinimized) i.showByUser() else i.hideByUser())
    }
  }, onBtnUp = m => {
    if(drawMapMode) {
      if(right_down_corner.nonEmpty) {
        for {
          x <- left_up_corner
          y <- right_down_corner
          c = (y - x).n * (y.dist(x) / 2f) + x
          h = math.abs(y.y - x.y)
          if h*globalScale > 10
        } {
          globalScale = math.min(1000000, 750 / h)
          _center = c
        }
      } else {
        InterfaceHolder.determineInterfaceElem(m) match {
          case Some(i) =>
            if(i.isMinimized) i.showByUser() else i.hideByUser()
          case None =>
            set_stop_moment = true
        }
      }
      left_up_corner = None
      right_down_corner = None
    }})
  leftMouseDragIgnorePause(onDrag = m => if(drawMapMode) {
    right_down_corner = Some(absCoord(m))
  })

  rightMouseIgnorePause(onBtnDown = m => {
    _stop_after_number_of_tacts = 0
  })

  //actionIgnorePause {
    //if(future_trajectory.isEmpty) updateFutureTrajectory("future_trajectory.isEmpty")
  //}

  /*actionIgnorePause(100) {
    if(continue_future_trajectory) continueFutureTrajectory()
  }*/

  action {
    //future_trajectory --= future_trajectory.takeWhile(_._1 < _tacts)
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
  globalScale = 10

  private val scale = 1e-6

  /*private def reducePointsNumber(points:Seq[DVec], res:List[DVec] = Nil):List[DVec] = {
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
  }*/

  private def shipOrbitRender(ship_index:String, color1:ScageColor, color2:ScageColor):() => Unit = {
    val result = ArrayBuffer[() => Unit]()
    // находим наш корабль
    system_evolution.bodyState(ship_index) match {
      case Some(bs) =>
        // смотрим, где он находится
        insideSphereOfInfluenceOfCelestialBody(bs.coord, bs.mass, system_evolution.bodyStates(planet_indexes)) match {
          case Some((planet, planet_state)) =>
            // корабль находится внутри гравитационного радиуса какой-то планеты (Земли или Луны)
            val orbit = calculateOrbit(
              planet_state.mass,
              planet_state.coord,
              bs.mass,
              bs.coord - planet_state.coord,
              bs.vel - planet_state.vel, G)
            orbit match {
              case h:HyperbolaOrbit =>
                val yy = (-math.acos(-1.0/h.e)+0.1 to math.acos(-1.0/h.e)-0.1 by 0.1).map(true_anomaly => {
                  val r = h.a*(h.e*h.e-1)/(1 + h.e*math.cos(true_anomaly))
                  (h.f + (h.f_minus_center_n*r).rotateRad(true_anomaly))*scale
                }).toList
                result += (() => {
                  drawSlidingLines(yy, color1)

                  if(ship_index == ship.index) {
                    val mouse_point = absCoord(mouseCoord) / scale
                    drawLine(h.f * scale, mouse_point * scale, DARK_GRAY)

                    if(_stop_after_number_of_tacts > 0) {
                      drawFilledCircle(h.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly)*scale, 3 / globalScale, RED)
                    }

                    val mouse_teta_rad2Pi = h.tetaRad2PiInPoint(mouse_point)
                    val ship_teta_rad2Pi = h.tetaRad2PiInPoint(bs.coord)
                    if(h.tetaRad2PiValid(mouse_teta_rad2Pi)) {
                      val ccw = (bs.coord - h.f).perpendicular * (bs.vel - planet_state.vel) >= 0 // летим против часовой?
                      val away_from_rp = (bs.coord - h.f) * (bs.vel - planet_state.vel) >= 0 // приближаемся к перигею или удаляемся от него?

                      val (allow) = {
                        if(ccw) {
                          (away_from_rp  && ship_teta_rad2Pi <= mouse_teta_rad2Pi && mouse_teta_rad2Pi <= h.teta_rad_min) ||
                          (!away_from_rp && ((ship_teta_rad2Pi <= mouse_teta_rad2Pi && mouse_teta_rad2Pi <= 360) || (0 <= mouse_teta_rad2Pi && mouse_teta_rad2Pi <= h.teta_rad_min)))
                        } else {
                          (away_from_rp && ship_teta_rad2Pi >= mouse_teta_rad2Pi && mouse_teta_rad2Pi >= h.teta_rad_max) ||
                          (!away_from_rp && ((ship_teta_rad2Pi >= mouse_teta_rad2Pi && mouse_teta_rad2Pi >= 0) || (360 >= mouse_teta_rad2Pi && mouse_teta_rad2Pi >= h.teta_rad_max)))
                        }
                      }

                      if(allow) {
                        val orbital_point = h.orbitalPointInPoint(mouse_point)
                        drawFilledCircle(orbital_point*scale, 3 / globalScale, color2)
                        val flight_time_msec = if (ccw) h.travelTimeOnOrbitMsecCCW(bs.coord, orbital_point) else h.travelTimeOnOrbitMsecCW(bs.coord, orbital_point)
                        val flight_time = s"${timeStr(flight_time_msec)}"
                        if (set_stop_moment) {
                          _stop_after_number_of_tacts = (flight_time_msec / 1000 / base_dt).toLong
                          _stop_in_orbit_true_anomaly = mouse_teta_rad2Pi
                          set_stop_moment = false
                        }
                        openglLocalTransform {
                          openglMove(orbital_point * scale)
                          print(s"  $flight_time : ${mOrKm(h.distanceByTrueAnomalyRad(mouse_teta_rad2Pi) - planet.radius)}", Vec.zero, size = (max_font_size / globalScale).toFloat, color2)
                        }
                      }
                    }
                  }
                })
              case e:EllipseOrbit =>
                result += (() => {
                  openglLocalTransform {
                    openglMove(e.center * scale)
                    openglRotateDeg(Vec(-1, 0).signedDeg(e.f2 - e.f))
                    drawEllipse(DVec.zero, e.a * scale, e.b * scale, color2)
                  }
                  if(ship_index == ship.index) {
                    val mouse_point = absCoord(mouseCoord) / scale
                    drawLine(e.f*scale, mouse_point*scale, DARK_GRAY)

                    val orbital_point = e.orbitalPointInPoint(mouse_point)
                    drawFilledCircle(orbital_point*scale, 3 / globalScale, color2)

                    /*if(e.r_p < planet.radius) {
                      val fall_teta = -math.acos((e.p/planet.radius - 1)/e.e)/math.Pi*180 + 360
                      val orbital_point2 = e.orbitalPointByTrueAnomalyDeg(fall_teta)
                      drawFilledCircle(orbital_point2*scale, 3 / globalScale, color2)
                    }*/

                    if(_stop_after_number_of_tacts > 0) {
                      drawFilledCircle(e.orbitalPointByTrueAnomalyRad(_stop_in_orbit_true_anomaly)*scale, 3 / globalScale, RED)
                    }
                    val true_anomaly_rad = e.tetaRad2PiInPoint(mouse_point)
                    val ccw = (bs.coord - e.f).perpendicular*(bs.vel - planet_state.vel) >= 0 // летим против часовой?
                    val flight_time_msec = if(ccw) e.travelTimeOnOrbitMsecCCW(bs.coord, orbital_point) else e.travelTimeOnOrbitMsecCW(bs.coord, orbital_point)
                    val flight_time = s"${timeStr(flight_time_msec)}"
                    if(set_stop_moment) {
                      _stop_after_number_of_tacts = (flight_time_msec/1000/base_dt).toLong
                      _stop_in_orbit_true_anomaly = true_anomaly_rad
                      set_stop_moment = false
                    }
                    /*val (vt, vr) = e.orbitalVelocityByTrueAnomalyRad(true_anomaly_rad)
                    val basis_r = (orbital_point - e.f).n
                    val basis_t = basis_r.p*/
                    /*val vnorm = math.sqrt(vr*vr + vt*vt)*/
                    //val v = vr*basis_r + vt*basis_t + planet_state.vel
                    openglLocalTransform {
                      openglMove(orbital_point * scale)
                      print(s"  $flight_time : ${mOrKm(e.distanceByTrueAnomalyRad(true_anomaly_rad) - planet.radius)}", Vec.zero, size = (max_font_size / globalScale).toFloat, color2)
                      //print(s"  $flight_time : ${msecOrKmsec(v.norma)}", Vec.zero, size = (max_font_size / globalScale).toFloat, color2)
                      //print(f"  ${e.tetaDeg360InPoint(x)}%.2f", Vec.zero, size = (max_font_size / globalScale).toFloat, color2)
                    }
                  }
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
  private var moon_orbit_render:() => Unit = () => {}

  private var _calculate_orbits = false

  /*private var _rendering_enabled = true
  def renderingEnabled = _rendering_enabled*/

  private def updateOrbits() {
    //println("updateOrbits")
    if(ship.flightMode != 0) {
      ship_orbit_render = if(ship.engines.exists(_.active)) {
        /*if(!onPause) {*/
          shipOrbitRender(ship.index, ship.colorIfAliveOrRed(PURPLE), RED)
        /*} else {
          // получаем состояние системы, когда двигатели отработали
          val system_state_when_engines_off = getFutureState(ship.engines.map(_.stopMomentTacts).max)
          shipOrbitRender(ship.index, system_state_when_engines_off, PURPLE, RED)
        }*/
      } else {
        // двигатели корабля не работают - можем работать с текущим состоянием
        shipOrbitRender(ship.index, ship.colorIfAliveOrRed(ORANGE), ship.colorIfAliveOrRed(YELLOW))
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
    } else {
      ship_orbit_render = if(ship.engines.exists(_.active)) {
        shipOrbitRender(ship.index, PURPLE, RED)
      } else {
        shipOrbitRender(ship.index, ship.colorIfAliveOrRed(ORANGE), ship.colorIfAliveOrRed(YELLOW))
      }
    }
    station_orbit_render = shipOrbitRender(station.index, ORANGE, YELLOW)
    moon_orbit_render =  shipOrbitRender(moon.index, GREEN, GREEN)
    _calculate_orbits = false
  }
  updateOrbits()

  actionStaticPeriodIgnorePause(1000) {
    if(/*_rendering_enabled && */drawMapMode && (!onPause || _calculate_orbits)) {
      updateOrbits()
    }
  }

  actionStaticPeriodIgnorePause(10000) {
    if(OrbitalKiller.timeMultiplier != realtime && OrbitalKiller.timeMultiplier > 1f*OrbitalKiller.timeMultiplier/63*OrbitalKiller.ticks + 20) {
      println("updating timeMultiplier")
      OrbitalKiller.timeMultiplier = (OrbitalKiller.timeMultiplier*1f/63*OrbitalKiller.ticks).toInt
    }
  }

  render {
    /*if(_rendering_enabled) {*/
      if(drawMapMode) {

        /*val spaces = splitSpace(new Space(our_mutable_system.map(_._1), DVec(OrbitalKiller.earth.radius*2, -OrbitalKiller.earth.radius*2)), 5, 2)
        spaces.foreach(s => {
          drawRectCentered(s.center*scale, s.width*scale, s.height*scale, GRAY)
        })
        println(spaces.filter(_.bodies.length > 1).map(x => s"${x.bodies.length}").mkString(" : "))*/

        drawCircle(earth.coord*scale, earth.radius * scale, WHITE)
        drawCircle(earth.coord*scale, equalGravityRadius(earth.currentState, moon.currentState)*scale, color = DARK_GRAY)
        drawLine(earth.coord*scale, (earth.coord*scale + DVec(0, earth.radius*scale)).rotateDeg(earth.currentState.ang), WHITE)
        /*openglLocalTransform {
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
        }*/

        drawCircle(moon.coord*scale, moon.radius * scale, WHITE)
        drawCircle(moon.coord*scale, equalGravityRadius(moon.currentState, earth.currentState)*scale, color = DARK_GRAY)
        //drawCircle(moon.coord*scale, soi(moon.mass, earth.coord.dist(moon.coord), earth.mass)*scale, color = DARK_GRAY)
        drawLine(moon.coord*scale, moon.coord * scale + DVec(0, moon.radius * scale).rotateDeg(moon.currentState.ang), WHITE)
        moon_orbit_render()
        if(!ship.isRemoved) {
          drawFilledCircle(ship.coord * scale, earth.radius * scale / 2f / globalScale, WHITE)
          ship_orbit_render()
        }
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

        if(left_up_corner.isEmpty) {
          val m = absCoord(mouseCoord)
          val d = (ship.coord * scale).dist(m) / scale
          drawArrow(ship.coord * scale, m, DARK_GRAY)
          openglLocalTransform {
            openglMove(m)
            print(s"  ${mOrKm(d.toLong)}", Vec.zero, size = (max_font_size / globalScale).toFloat, DARK_GRAY)
          }
        }
      } else {
        if(!ship.isRemoved) {
          val m = absCoord(mouseCoord)
          val d = ship.coord.dist(m)
          openglLocalTransform {
            openglMove(ship.coord - base)
            drawArrow(DVec.zero, m - ship.coord, DARK_GRAY)
            openglMove(m - ship.coord)
            openglRotateDeg(-rotationAngleDeg)
            print(s"  ${mOrKm(d.toLong)}", Vec.zero, size = (max_font_size / globalScale).toFloat, DARK_GRAY)
          }
        }
      }
    /*}*/
  }

  interface {
    if(onPause) print("Пауза", windowCenter.toVec, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)
    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$ticks", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action ${averageRenderTimeMsec*fps/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%%/${1*averageActionTimeMsec*ticks/(averageRenderTimeMsec*fps+averageActionTimeMsec*ticks)*100}%.2f%%", windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(f"Render/Action $averageRenderTimeMsec%.2f msec/$averageActionTimeMsec%.2f msec", windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)
    print(s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec", windowWidth - 20, windowHeight - 100, align = "top-right", color = DARK_GRAY)

    /*if(_rendering_enabled) {*/
      val a = DVec(windowWidth - 250, 20)
      val b = DVec(windowWidth - 250 + 100, 20)
      drawLine(a,b, DARK_GRAY)
      drawLine(a, a+(a-b).rotateDeg(90).n*5, DARK_GRAY)
      drawLine(b, b+(a-b).rotateDeg(90).n*5, DARK_GRAY)
      print(s"${mOrKm((100/globalScale/(if(drawMapMode) scale else 1.0)).toInt)}", b.toVec, DARK_GRAY)
    /*}*/

    /*if(!disable_interface_drawing) {*/
      /*if(_rendering_enabled) {*/
        /*openglLocalTransform {
          val z = windowHeight/2f-40f
          openglMove(windowCenter)
          openglRotateDeg(rotationAngleDeg)
          drawArrow(DVec(0, -z), DVec(0, z), DARK_GRAY, 1)
          print("y", Vec(0, z), DARK_GRAY)
          drawArrow(DVec(-z, 0), DVec(z, 0), DARK_GRAY, 1)
          print("x", Vec(z, 0), DARK_GRAY)
        }*/
      /*}*/

      InterfaceHolder.update()
      InterfaceHolder.strings.zipWithIndex.foreach {
        case ((str, color), idx) => print(str, 20, (InterfaceHolder.strings.length+2 - idx)*20, ship.colorIfAliveOrRed(color))
      }
      //print(InterfaceHolder.minimizedStrings.map(_._1).mkString(" "), 20, 20, DARK_GRAY)
      InterfaceHolder.minimizedStrings.zipWithIndex.foreach {
        case ((i, color), idx) =>
          print(i.shortDescr, 20+idx*40, 20, ship.colorIfAliveOrRed(color), align = "center")
      }
    /*}*/
  }

  pause()
}

import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

trait CelestialBody {
  def index:String
  def name:String
  def coord:DVec
  def linearVelocity:DVec
  def mass:Double
  def radius:Double
  def initState:BodyState

  val currentState:MutableBodyState = initState.toMutableBodyState
  val ground_length_km = (2*math.Pi*radius/1000).toInt
  def groundSpeedMsec = currentState.ang_vel.toRad*radius

  val g = G*mass/(radius*radius)  // ускорение свободного падения, м/с^2
}

class Planet(
  val index:String,
  val name:String,
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

  // рельеф планеты: треугольные горы. два параметра: высота в метрах, ширина основания в метрах
  private val ground_features = Array.ofDim[(Int, Int)](ground_length_km)
  (0 until ground_length_km).foreach(x => ground_features(x) = (100 + (math.random*900).toInt, 50 + (math.random*300).toInt))

  private def groundFeatureNear(point_km:Double):(Int, Int) = {
    if(point_km < 0) groundFeatureNear(point_km + ground_length_km)
    else if(point_km >= ground_length_km) groundFeatureNear(point_km - ground_length_km)
    else ground_features(point_km.toInt)
  }

  private var data_initialized = false
  private val half_render_length_km = 50
  private val alpha = half_render_length_km * 2.0 * 1000 * 180 / math.Pi / radius
  private var viewpoint_dist:Double = _
  private var to_viewpoint:DVec = _
  private var points:Seq[DVec] = _
  private var ground_position_ang:Double = _
  private var ground_position_km:Double = _
  private var ground_features_near:Seq[(DVec, Int, Int)] = _

  private def updateRenderData() {
    viewpoint_dist = math.abs((ship.coord + shipOffset).dist(coord) - radius)
    if(viewpoint_dist < 50000) {
      //val before_to_viewpoint = (ship.coord + shipOffset - coord).n*(radius - 1000)
      to_viewpoint = (ship.coord + shipOffset - coord).n * radius
      points = for {
        ang <- -alpha to alpha by 0.01
        point = to_viewpoint.rotateDeg(ang)
      } yield point
      ground_position_ang = correctAngle(DVec(0, 1).deg360(to_viewpoint) - currentState.ang)
      ground_position_km = {
        val x = ground_position_ang / 360.0 * 2 * math.Pi * radius / 1000
        if(x - half_render_length_km < 0) x + ground_length_km else x
      }
      ground_features_near = for {
        real_point <- ground_position_km - half_render_length_km to ground_position_km + half_render_length_km-1 by 1.0
        (w, h) = groundFeatureNear(real_point)
        point_ang = (360.0*real_point.toInt/ground_length_km) - ground_position_ang
        p = to_viewpoint.rotateDeg(point_ang)
      } yield (p, w, h)
    }
  }

  action(1) {
    updateRenderData()
  }

  actionIgnorePause(1) {
    updateRenderData()
    data_initialized = true
    deleteSelf()
  }

  render {
    if(data_initialized && /*renderingEnabled &&*/ !drawMapMode) {
      if(viewpoint_dist < 500) {
        openglLocalTransform {
          openglMove(coord - base)

          ground_features_near.foreach { case (p, w, h) =>
            drawLine(p + p.p*w/2, p + p.n*h, WHITE)
            drawLine(p - p.p*w/2, p + p.n*h, WHITE)
            drawLine(p,                p + p.n*h, WHITE)
          }
        }
        openglLocalTransform {
          openglMove(ship.coord - base)
          val pa = (coord - ship.coord).n*(ship.coord.dist(coord) - radius) + (coord - ship.coord).p*70000
          val pb = (coord - ship.coord).n*(ship.coord.dist(coord) - radius) + (coord - ship.coord).p*(-70000)
          drawLine(pa, pb, WHITE)
        }
      } else if(viewpoint_dist < 50000) {
        openglLocalTransform {
          openglMove(coord - base)
          drawSlidingLines(points, WHITE)
          ground_features_near.foreach { case (p, w, h) =>
            drawLine(p + p.p*w/2, p + p.n*h, WHITE)
            drawLine(p - p.p*w/2, p + p.n*h, WHITE)
            drawLine(p,                p + p.n*h, WHITE)
          }
        }
      }
    }
  }
}

class Star(val index:String, val name:String, val mass:Double, val coord:DVec, ang_vel:Double, val radius:Double) extends CelestialBody {
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

  private var data_initialized = false
  private val alpha = 100 * 1000 * 180 / math.Pi / radius // угловой размер для 100 километров поверхности
  private var viewpoint_dist:Double = _
  private var to_viewpoint:DVec = _
  private var points:Seq[DVec] = _

  private def updateRenderData(): Unit = {
    viewpoint_dist = math.abs((ship.coord + shipOffset).dist(coord) - radius)
    if (viewpoint_dist < 50000) {
      to_viewpoint = (ship.coord + shipOffset - coord).n * radius
      points = for {
        ang <- -alpha to alpha by 0.01
        point = to_viewpoint.rotateDeg(ang)
      } yield point
    }
  }

  action(1) {
    updateRenderData()
  }

  actionIgnorePause(1) {
    updateRenderData()
    data_initialized = true
    deleteSelf()
  }

  render {
    if (data_initialized && /*renderingEnabled &&*/ !drawMapMode && viewpoint_dist < 50000) {
      openglLocalTransform {
        openglMove(coord - base)
        drawSlidingLines(points, WHITE)
      }
    }
  }

  def linearVelocity: DVec = DVec.dzero
}






