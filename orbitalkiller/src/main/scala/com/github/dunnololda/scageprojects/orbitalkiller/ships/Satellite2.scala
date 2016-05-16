package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.{DockingPoints, PolygonShape, PolygonShip, Engine}

class Satellite2(index: Int,
                 init_coord: DVec,
                 init_velocity: DVec = DVec.dzero,
                 init_rotation: Double = 0.0,
                 ship_designer:Boolean = false) extends PolygonShip(index, "Любопытный", init_coord, init_velocity, init_rotation, ship_designer, true) {
  private val _payload:Double = 5*1000
  private var _fuel_mass:Double = 5*1000
  def mass:Double = _payload + _fuel_mass
  override def fuelMass: Double = _fuel_mass
  override def fuelMass_=(m: Double): Unit = {_fuel_mass = m}

  lazy val engine_size:Double = 2.5

  lazy val points:List[DVec] = List(
    DVec(7.5, -12.5),
    DVec(12.5, -7.5),
    DVec(12.5, -2.5),
    DVec(17.5, -2.5),
    DVec(17.5, -7.5),
    DVec(62.5, -7.5),
    DVec(62.5, 7.5),
    DVec(17.5, 7.5),
    DVec(17.5, 2.5),
    DVec(12.5, 2.5),
    DVec(12.5, 12.5),
    DVec(7.5, 22.5),
    DVec(7.5, 27.5),
    DVec(2.5, 37.5),
    DVec(2.5, 52.5),
    DVec(-2.5, 52.5),
    DVec(-2.5, 37.5),
    DVec(-7.5, 27.5),
    DVec(-7.5, 22.5),
    DVec(-12.5, 12.5),
    DVec(-12.5, 2.5),
    DVec(-17.5, 2.5),
    DVec(-17.5, 7.5),
    DVec(-62.5, 7.5),
    DVec(-62.5, -7.5),
    DVec(-17.5, -7.5),
    DVec(-17.5, -2.5),
    DVec(-12.5, -2.5),
    DVec(-12.5, -7.5),
    DVec(-7.5, -12.5)
  )

  lazy val convex_parts = List(
    PolygonShape(List(DVec(-62.5, 7.5), DVec(-62.5, -7.5), DVec(-17.5, -7.5), DVec(-17.5, 7.5)), Nil),
    PolygonShape(List(DVec(-17.5, -2.5), DVec(-12.5, -2.5), DVec(-12.5, 2.5), DVec(-17.5, 2.5)), Nil),
    PolygonShape(List(DVec(-12.5, -7.5), DVec(-7.5, -12.5), DVec(7.5, -12.5), DVec(12.5, -7.5)), Nil),
    PolygonShape(List(DVec(-12.5, 12.5), DVec(-12.5, -7.5), DVec(12.5, -7.5), DVec(12.5, 12.5)), Nil),
    PolygonShape(List(DVec(-12.5, 12.5), DVec(12.5, 12.5), DVec(7.5, 22.5), DVec(-7.5, 22.5)), Nil),
    PolygonShape(List(DVec(-7.5, 22.5), DVec(7.5, 22.5), DVec(7.5, 27.5), DVec(-7.5, 27.5)), Nil),
    PolygonShape(List(DVec(-7.5, 27.5), DVec(7.5, 27.5), DVec(2.5, 37.5), DVec(-2.5, 37.5)), Nil),
    PolygonShape(List(DVec(-2.5, 37.5), DVec(2.5, 37.5), DVec(2.5, 52.5), DVec(-2.5, 52.5)), Nil),
    PolygonShape(List(DVec(12.5, 2.5), DVec(12.5, -2.5), DVec(17.5, -2.5), DVec(17.5, 2.5)), Nil),
    PolygonShape(List(DVec(17.5, -7.5), DVec(62.5, -7.5), DVec(62.5, 7.5), DVec(17.5, 7.5)), Nil)
  )

  val wreck_parts = List(
    PolygonShape(List(DVec(-62.5, 7.5), DVec(-62.5, -7.5), DVec(-52.5, -7.5), DVec(-57.5, 2.5)), Nil),  // 1
    PolygonShape(List(DVec(-62.5, 7.5), DVec(-57.5, 2.5), DVec(-42.5, 2.5), DVec(-37.5, 7.5)), Nil),  // 2
    PolygonShape(List(DVec(-57.5, 2.5), DVec(-52.5, -7.5), DVec(-37.5, -7.5), DVec(-52.5, 2.5)), Nil),  // 3
    PolygonShape(List(DVec(-52.5, 2.5), DVec(-37.5, -7.5), DVec(-42.5, 2.5)), Nil),  // 4
    PolygonShape(List(DVec(-42.5, 2.5), DVec(-37.5, -7.5), DVec(-32.5, -7.5), DVec(-37.5, 7.5)), Nil),  // 5
    PolygonShape(List(DVec(-37.5, 7.5), DVec(-32.5, -7.5), DVec(-27.5, -7.5), DVec(-32.5, 2.5)), Nil),  // 6
    PolygonShape(List(DVec(-37.5, 7.5), DVec(-32.5, 2.5), DVec(-22.5, 2.5), DVec(-17.5, 7.5)), Nil),  // 7
    PolygonShape(List(DVec(-32.5, 2.5), DVec(-27.5, -7.5), DVec(-17.5, -7.5), DVec(-22.5, 2.5)), Nil),  // 8
    PolygonShape(List(DVec(-22.5, 2.5), DVec(-17.5, -7.5), DVec(-17.5, 7.5)), Nil),  // 9
    PolygonShape(List(DVec(-17.5, 2.5), DVec(-17.5, -2.5), DVec(2.5, -2.5)), Nil),  // 10
    PolygonShape(List(DVec(-17.5, 2.5), DVec(2.5, -2.5), DVec(2.5, 2.5)), Nil),  // 11
    PolygonShape(List(DVec(-12.5, -2.5), DVec(-12.5, -7.5), DVec(-7.5, -12.5), DVec(2.5, -7.5)), Nil),  // 12
    PolygonShape(List(DVec(-12.5, -2.5), DVec(2.5, -7.5), DVec(12.5, -2.5)), Nil),  // 13
    PolygonShape(List(DVec(-7.5, -12.5), DVec(7.5, -12.5), DVec(12.5, -7.5), DVec(12.5, -2.5)), Nil),  // 14
    PolygonShape(List(DVec(-12.5, 12.5), DVec(-12.5, 2.5), DVec(2.5, 2.5), DVec(-7.5, 12.5)), Nil),  // 15
    PolygonShape(List(DVec(-12.5, 12.5), DVec(-2.5, 12.5), DVec(-7.5, 22.5)), Nil),  // 16
    PolygonShape(List(DVec(-7.5, 22.5), DVec(2.5, 22.5), DVec(-7.5, 27.5)), Nil),  // 17
    PolygonShape(List(DVec(-7.5, 27.5), DVec(2.5, 22.5), DVec(7.5, 27.5), DVec(-2.5, 37.5)), Nil),  // 18
    PolygonShape(List(DVec(-7.5, 22.5), DVec(-2.5, 12.5), DVec(2.5, 12.5), DVec(2.5, 22.5)), Nil),  // 19
    PolygonShape(List(DVec(-7.5, 12.5), DVec(2.5, 2.5), DVec(7.5, 7.5), DVec(2.5, 12.5)), Nil),  // 20
    PolygonShape(List(DVec(2.5, 2.5), DVec(2.5, -2.5), DVec(17.5, -2.5)), Nil),  // 21
    PolygonShape(List(DVec(2.5, 2.5), DVec(17.5, -2.5), DVec(27.5, 2.5)), Nil),  // 22
    PolygonShape(List(DVec(2.5, 2.5), DVec(12.5, 2.5), DVec(12.5, 7.5), DVec(7.5, 7.5)), Nil),  // 23
    PolygonShape(List(DVec(2.5, 12.5), DVec(7.5, 7.5), DVec(12.5, 7.5), DVec(12.5, 12.5), DVec(7.5, 22.5)), Nil),  // 24
    PolygonShape(List(DVec(2.5, 22.5), DVec(2.5, 12.5), DVec(7.5, 22.5), DVec(7.5, 27.5)), Nil),  // 25
    PolygonShape(List(DVec(-2.5, 42.5), DVec(-2.5, 37.5), DVec(7.5, 27.5), DVec(2.5, 37.5), DVec(2.5, 42.5)), List(   // 26 NOT CONVEX
      PolygonShape(List(DVec(-2.5, 42.5), DVec(-2.5, 37.5), DVec(2.5, 37.5), DVec(2.5, 42.5)), Nil),
      PolygonShape(List(DVec(-2.5, 37.5), DVec(7.5, 27.5), DVec(2.5, 37.5)), Nil)
    )),
    PolygonShape(List(DVec(-2.5, 52.5), DVec(-2.5, 42.5), DVec(2.5, 42.5)), Nil),  // 27
    PolygonShape(List(DVec(-2.5, 52.5), DVec(2.5, 42.5), DVec(2.5, 52.5)), Nil),  // 28
    PolygonShape(List(DVec(17.5, 7.5), DVec(17.5, 2.5), DVec(37.5, 2.5), DVec(37.5, 7.5)), Nil),  // 29
    PolygonShape(List(DVec(17.5, -2.5), DVec(17.5, -7.5), DVec(27.5, -7.5), DVec(27.5, 2.5)), Nil),  // 30
    PolygonShape(List(DVec(27.5, -7.5), DVec(37.5, -7.5), DVec(32.5, -2.5), DVec(27.5, -2.5)), Nil),  // 31
    PolygonShape(List(DVec(27.5, 2.5), DVec(27.5, -2.5), DVec(32.5, -2.5), DVec(37.5, -7.5), DVec(37.5, -2.5)), List(   // 32 NOT CONVEX
      PolygonShape(List(DVec(-2.5, 42.5), DVec(-2.5, 37.5), DVec(2.5, 37.5), DVec(2.5, 42.5)), Nil),
      PolygonShape(List(DVec(-2.5, 37.5), DVec(7.5, 27.5), DVec(2.5, 37.5)), Nil)
    )),
    PolygonShape(List(DVec(27.5, 2.5), DVec(37.5, -2.5), DVec(37.5, 2.5)), Nil),  // 33
    PolygonShape(List(DVec(37.5, 7.5), DVec(37.5, -7.5), DVec(47.5, -7.5)), Nil),  // 34
    PolygonShape(List(DVec(37.5, 7.5), DVec(47.5, -7.5), DVec(52.5, -2.5), DVec(47.5, 2.5)), Nil),  // 35
    PolygonShape(List(DVec(37.5, 7.5), DVec(47.5, 2.5), DVec(62.5, 7.5)), Nil),  // 36
    PolygonShape(List(DVec(47.5, 2.5), DVec(52.5, -2.5), DVec(62.5, 2.5), DVec(62.5, 7.5)), Nil),  // 37
    PolygonShape(List(DVec(47.5, -7.5), DVec(57.5, -7.5), DVec(52.5, -2.5)), Nil),  // 38
    PolygonShape(List(DVec(52.5, -2.5), DVec(57.5, -7.5), DVec(62.5, -7.5), DVec(62.5, 2.5)), Nil)  // 39
  )

  val docking_points = List(new DockingPoints(DVec(1.5, 52.5), DVec(-1.5, 52.5), this, Some(8)))

  val eight = new Engine(8, position = DVec(0.0, 52.5), force_dir = DVec(0.0, -1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val two = new Engine(2, position = DVec(0.0, -12.5), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val four = new Engine(4, position = DVec(-62.5, 0.0), force_dir = DVec(1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val six = new Engine(6, position = DVec(62.5, 0.0), force_dir = DVec(-1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val seven = new Engine(7, position = DVec(-2.5, 50.0), force_dir = DVec(1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val nine = new Engine(9, position = DVec(2.5, 50.0), force_dir = DVec(-1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)

  val engines = List(eight, two, four, six, seven, nine)

  val engines_mapping = Map(
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine
  )

  def preserveVelocity(vel:DVec) {}
  def preserveAngularVelocity(ang_vel_deg: Double) {}

  override val is_manned: Boolean = false
}
