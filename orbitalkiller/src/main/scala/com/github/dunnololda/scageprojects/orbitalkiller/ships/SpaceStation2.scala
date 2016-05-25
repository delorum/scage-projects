package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller._

class SpaceStation2(index: Int,
                    init_coord: DVec,
                    init_velocity: DVec = DVec.dzero,
                    init_rotation: Double = 0.0,
                    ship_designer:Boolean = false) extends PolygonShip(index, "Станция Огонек", init_coord, init_velocity, init_rotation, ship_designer, true) {
  private val _payload: Double = 50 * 1000
  private var _fuel_mass: Double = 50 * 1000

  def mass: Double = _payload + _fuel_mass

  override def fuelMass: Double = _fuel_mass

  override def fuelMass_=(m: Double): Unit = {
    _fuel_mass = m
  }

  val is_manned = true
  override val pilot_position = DVec(-120, 0)

  lazy val engine_size: Double = 10

  lazy val points: List[DVec] = List(
    DVec(-90.0, -10.0),
    DVec(-130.0, -10.0),
    DVec(-130.0, 10.0),
    DVec(-90.0, 10.0),
    DVec(-50.0, 30.0),
    DVec(50.0, 30.0),
    DVec(90.0, 10.0),
    DVec(130.0, 10.0),
    DVec(130.0, -10.0),
    DVec(90.0, -10.0),
    DVec(50.0, -30.0),
    DVec(-50.0, -30.0)
  )

  lazy val convex_parts = List(
    PolygonShape(List(DVec(-130.0, -10.0), DVec(-90.0, -10.0), DVec(-90.0, 10.0), DVec(-130.0, 10.0)), Nil),
    PolygonShape(List(DVec(-90.0, -10.0), DVec(-50.0, -30.0), DVec(-50.0, 30.0), DVec(-90.0, 10.0)), Nil),
    PolygonShape(List(DVec(-50.0, -30.0), DVec(50.0, -30.0), DVec(50.0, 30.0), DVec(-50.0, 30.0)), Nil),
    PolygonShape(List(DVec(50.0, -30.0), DVec(90.0, -10.0), DVec(90.0, 10.0), DVec(50.0, 30.0)), Nil),
    PolygonShape(List(DVec(90.0, -10.0), DVec(130.0, -10.0), DVec(130.0, 10.0), DVec(90.0, 10.0)), Nil)
  )

  val wreck_parts = List(
    PolygonShape(List(DVec(-130.0, -10.0), DVec(-110.0, -10.0), DVec(-120.0, 10.0), DVec(-130.0, 10.0)), Nil),
    PolygonShape(List(DVec(-110.0, -10.0), DVec(-90.0, -10.0), DVec(-90.0, 10.0), DVec(-110.0, 10.0)), Nil),
    PolygonShape(List(DVec(-120.0, 10.0), DVec(-110.0, -10.0), DVec(-110.0, 10.0)), Nil),
    PolygonShape(List(DVec(-90.0, -10.0), DVec(-70.0, -20.0), DVec(-90.0, 10.0)), Nil),
    PolygonShape(List(DVec(-70.0, -20.0), DVec(-50.0, -30.0), DVec(-50.0, 30.0), DVec(-90.0, 10.0)), Nil),
    PolygonShape(List(DVec(-50.0, -30.0), DVec(-20.0, -30.0), DVec(-10.0, 0.0), DVec(-50.0, 0.0)), Nil),
    PolygonShape(List(DVec(-50.0, 30.0), DVec(-50.0, 0.0), DVec(-30.0, 0.0), DVec(-20.0, 30.0)), Nil),
    PolygonShape(List(DVec(-30.0, 0.0), DVec(0.0, 0.0), DVec(10.0, 30.0), DVec(-20.0, 30.0)), Nil),
    PolygonShape(List(DVec(-20.0, -30.0), DVec(20.0, -30.0), DVec(20.0, 0.0), DVec(-10.0, 0.0)), Nil),
    PolygonShape(List(DVec(0.0, 0.0), DVec(20.0, 0.0), DVec(30.0, 30.0), DVec(10.0, 30.0)), Nil),
    PolygonShape(List(DVec(20.0, -30.0), DVec(50.0, -30.0), DVec(50.0, 0.0), DVec(20.0, 0.0)), Nil),
    PolygonShape(List(DVec(20.0, 0.0), DVec(90.0, 0.0), DVec(90.0, 10.0), DVec(50.0, 30.0)), Nil),
    PolygonShape(List(DVec(30.0, 30.0), DVec(20.0, 0.0), DVec(50.0, 30.0)), Nil),
    PolygonShape(List(DVec(50.0, -30.0), DVec(90.0, -10.0), DVec(90.0, 0.0), DVec(50.0, 0.0)), Nil),
    PolygonShape(List(DVec(90.0, -10.0), DVec(100.0, -10.0), DVec(120.0, 10.0), DVec(90.0, 10.0)), Nil),
    PolygonShape(List(DVec(100.0, -10.0), DVec(130.0, -10.0), DVec(130.0, 10.0), DVec(120.0, 10.0)), Nil)
  )

  val docking_points = List(new DockingPoints(DVec(-130.0, 1.5), DVec(-130.0, -1.5), this, Some(4), Nil),
    new DockingPoints(DVec(130.0, -1.5), DVec(130.0, 1.5), this, Some(6), Nil))

  val four = new Engine(4, Vec(-130.0, 0.0), Vec(1.0, 0.0), 10, 1, 4, this)
  val six = new Engine(6, Vec(130.0, 0.0), Vec(-1.0, 0.0), 10, 1, 4, this)
  val eight = new Engine(8, Vec(0.0, 30.0), Vec(0.0, -1.0), 10, 1, 4, this)
  val two = new Engine(2, Vec(0.0, -30.0), Vec(0.0, 1.0), 10, 1, 4, this)
  val one = new Engine(1, Vec(-120.0, -10.0), Vec(0.0, 1.0), 10, 1, 4, this)
  val three = new Engine(3, Vec(120.0, -10.0), Vec(0.0, 1.0), 10, 1, 4, this)

  val _engines = List(four, six, eight, two, one, three)

  val engines_by_keycodes_map = Map(
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

  def preserveVelocity(vel: DVec) {}

  def preserveAngularVelocity(ang_vel_deg: Double) {}
}
