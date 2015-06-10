package com.github.dunnololda.scageprojects.orbitalkiller.tests

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller._

import scala.collection.mutable.ArrayBuffer

object OrbitalOscillationsTests extends ScageApp {
  val earth = new Star("Earth", mass = 5.9746E24, coord = DVec.dzero, radius = 6400000)

  val moon_start_position = DVec(-269000000, 269000000)
  val moon_init_velocity = satelliteSpeed(moon_start_position, earth.coord, earth.linearVelocity, earth.mass, G, true)
  val moon = new Planet(
    "Moon",
    mass = 7.3477E22,
    init_coord = moon_start_position,
    init_velocity = moon_init_velocity,
    radius = 1737000)

  val planets = List(earth, moon)
  val planet_indexes = planets.map(_.index).toSet
  def currentPlanetStates = planets.map(_.currentState)
  def planetByIndex(index:String):Option[CelestialBody] = planets.find(_.index == index)

  /*val ship_start_position = earth.coord + DVec(0, earth.radius + 100000)
  val ship_init_velocity = satelliteSpeed(ship_start_position, earth.coord, earth.linearVelocity, earth.mass, G)*/
  /*val ship_start_position = moon_start_position + DVec(0, moon.radius + 100000)
  val ship_init_velocity = satelliteSpeed(ship_start_position, moon_start_position, moon_init_velocity, moon.mass, G)
  val ship = new Ship3("ship",
    init_coord = ship_start_position,
    init_velocity = ship_init_velocity,
    init_rotation = 45
  )*/

  val station_start_position = earth.coord + DVec(300, earth.radius + 100000)
  val station_init_velocity = satelliteSpeed(station_start_position, earth.coord, earth.linearVelocity, earth.mass, G, counterclockwise = true)*1.25
  val station = new SpaceStation("station",
    init_coord = station_start_position,
    init_velocity = station_init_velocity,
    init_rotation = 45
  )

  val ships = List(/*ship, */station)

  val k:Double = 1 // доля секунды симуляции, которая обрабатывается за один тик, если не применяется ускорение

  // движок делает вызов обработчика примерно 60 раз в секунду, за каждый вызов будет обрабатывать вот такую порцию симуляции
  // то есть, мы хотим, чтобы за одну реальную секунду обрабатывалось k секунд симуляции, поэтому за один такт движка (которых 60 в секунду)
  // будем обрабатывать k/60
  val base_dt:Double = 1.0/63*k

  val realtime = (1.0/k).toInt // 1/k*baseDt соответствует реальному течению времени

  def futureSystemEvolutionFrom(dt: => Double, tacts:Long, body_states:List[BodyState], enable_collisions:Boolean) = systemEvolutionFrom(
    dt, 1000000, base_dt,
    force = (tacts, bs, other_bodies) => {
      bs.index match {
        /*case ship.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
          other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero) +
          ship.currentReactiveForce(tacts, bs)*/
        case station.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G) +
          //other_bodies.find(_.index == moon.index).map(obs => gravityForce(obs.coord, obs.mass, bs.coord, bs.mass, G)).getOrElse(DVec.dzero) +
          station.currentReactiveForce(tacts, bs)
        case moon.index =>
          gravityForce(earth.coord, earth.mass, bs.coord, bs.mass, G)
        case _ => DVec.dzero
      }
    },
    torque = (tacts, bs, other_bodies) => {
      bs.index match {
        /*case ship.index =>
          ship.currentTorque(tacts, bs)*/
        case _ =>
          0
      }
    },
    enable_collisions = enable_collisions)((tacts, body_states))

  private val real_system_evolution =
    futureSystemEvolutionFrom(realtime*base_dt, 0, List(
      //ship.initState,
      station.initState,
      moon.initState,
      earth.initState),
      enable_collisions = false).iterator

  private var l = 1000
  private val system_evolution_lx =
    futureSystemEvolutionFrom(l*realtime*base_dt, 0, List(
      //ship.initState,
      station.initState,
      moon.initState,
      earth.initState),
      enable_collisions = false).iterator

  def mOrKm(meters:Number):String = {
    if(math.abs(meters.floatValue()) < 1000) f"${meters.floatValue()}%.2f м" else f"${meters.floatValue()/1000}%.2f км"
  }

  def msec2OrKmsec2(meters:Number):String = {
    if(math.abs(meters.floatValue()) < 1000) f"${meters.floatValue()}%.2f м/сек^2" else f"${meters.floatValue()/1000}%.2f км/сек^2"
  }

  def timeStr(time_msec:Long):String = {
    val msec  = 1l
    val sec  = msec*1000
    val min  = sec*60
    val hour  = min*60
    val day  = hour*24
    s"${time_msec/day} дн ${time_msec%day/hour} ч ${time_msec%hour/min} мин ${time_msec%min/sec} сек ${time_msec%sec/msec} мсек"
  }

  private var min_ship_moon:Double = 150000
  private var min_station_earth:Double = station_start_position.dist(earth.coord) - earth.radius + 100000
  private var min_station_earth2:Double = station_start_position.dist(earth.coord) - earth.radius + 100000

  private var max_cen_acc:Double = 0

  action {
    //val steps = (math.random*100+1).toInt
    val steps = 1
    (1 to steps*l-1).foreach(i => {
      val s = real_system_evolution.next()
      val station_state = s._2.find(_.index == station.index).get
      val earth_state = s._2.find(_.index == earth.index).get
      val station_earth = station_state.coord.dist(earth_state.coord) - earth.radius
      if(station_earth < min_station_earth) min_station_earth = station_earth
    })
    val s = real_system_evolution.next()

    /*val ship_state = s._2.find(_.index == ship.index).get
    val moon_state = s._2.find(_.index == moon.index).get
    val ship_moon = ship_state.coord.dist(moon_state.coord) - moon.radius
    if(ship_moon < min_ship_moon) min_ship_moon = ship_moon
    val ship_moon_deg = (ship_state.coord - moon_state.coord).deg(ship_state.vel - moon_state.vel)*/

    val station_state = s._2.find(_.index == station.index).get
    val earth_state = s._2.find(_.index == earth.index).get
    val station_earth = station_state.coord.dist(earth_state.coord) - earth.radius
    if(station_earth < min_station_earth) min_station_earth = station_earth
    val station_earth_deg = (station_state.coord - earth_state.coord).deg(station_state.vel)
    val dt = realtime*base_dt
    val a = (station_state.coord - earth_state.coord).deg(station_state.coord - earth_state.coord + station_state.vel*dt)

    val cen_acc = station_state.acc* station_state.vel.p

    val EllipseOrbit(_, _, e, _, _, r_p, r_a, t, _, _, _) = calculateOrbit(earth_state.mass, earth_state.coord, station_state.mass, station_state.coord - earth_state.coord, station_state.vel - earth_state.vel, G)

    //println(f"e = $e%.2f, r_p = ${mOrKm(r_p - earth.radius)}, r_a = ${mOrKm(r_a - earth.radius)}, t = ${timeStr((t*1000l).toLong)}")

    //println(s"${timeStr((s._1*base_dt*1000).toLong)} : ${mOrKm(ship_moon)} (${mOrKm(min_ship_moon)}, $ship_moon_deg) : ${mOrKm(station_earth)} (${mOrKm(min_station_earth)}, $station_earth_deg)")
    println(s"${timeStr((s._1*base_dt*1000).toLong)} : ${mOrKm(station_earth)} : ${msec2OrKmsec2(cen_acc)} (${mOrKm(min_station_earth)})")

    (1 to (steps-1)).foreach(i => {
      val s2 = system_evolution_lx.next()
      val station_state2 = s2._2.find(_.index == station.index).get
      val earth_state2 = s2._2.find(_.index == earth.index).get
      val station_earth2 = station_state2.coord.dist(earth_state2.coord) - earth.radius
      if(station_earth2 < min_station_earth2) min_station_earth2 = station_earth2
      /*l = {
        val a = station_state2.acc*station_state2.vel.p
        math.abs(math.pow(351.3011068768212 / a, 10.0 / 7).toInt)
      }*/
    })
    val s2 = system_evolution_lx.next()

    val station_state2 = s2._2.find(_.index == station.index).get
    val earth_state2 = s2._2.find(_.index == earth.index).get
    val station_earth2 = station_state2.coord.dist(earth_state2.coord) - earth.radius
    if(station_earth2 < min_station_earth2) min_station_earth2 = station_earth2
    val station_earth_deg2 = (station_state2.coord - earth_state2.coord).deg(station_state2.vel)
    val dt2 = l*realtime*base_dt
    val a2 = (station_state2.coord - earth_state2.coord).deg(station_state2.coord - earth_state2.coord + station_state2.vel*dt2)
    l = {
      val a = station_state2.acc*station_state2.vel.p
      math.abs(math.pow(351.3011068768212 / a, 10.0 / 7).toInt)
    }

    val cen_acc2 = station_state2.acc*station_state2.vel.p
    val cur = station_state2.vel.norma2/(station_state2.acc *station_state2.vel.p)

    val x = (math.abs(station_earth2 - station_earth)/station_earth*100)/cen_acc2
    val error = math.abs(station_earth2 - station_earth)/station_earth*100

    //println(s"${timeStr((s._1*base_dt*1000).toLong)} : ${mOrKm(ship_moon)} (${mOrKm(min_ship_moon)}, $ship_moon_deg) : ${mOrKm(station_earth)} (${mOrKm(min_station_earth)}, $station_earth_deg)")
    println(f"$l : ${timeStr((s2._1*base_dt*1000).toLong)} : ${mOrKm(station_earth2)} : ${math.abs(station_earth2 - station_earth)/station_earth*100}%.2f% : ${msec2OrKmsec2(cen_acc2)} : ${mOrKm(cur)} (${mOrKm(min_station_earth2)})")
    //println(f"${(s2._1*base_dt*1000).toLong} ${math.abs(station_earth2 - station_earth)/station_earth*100}%.2f $cen_acc2%.2f $cur%.2f")
    println("===================================")
    //println()
  }
}
