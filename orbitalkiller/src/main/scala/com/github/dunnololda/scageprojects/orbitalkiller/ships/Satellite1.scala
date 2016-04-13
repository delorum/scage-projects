package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

class Satellite1(index:Int,
                 init_coord:DVec,
                 init_velocity:DVec = DVec.dzero,
                 init_rotation:Double = 0.0) extends PolygonShip(index, "Светлячок", init_coord, init_velocity, init_rotation) {
  private val _payload:Double = 5*1000
  private var _fuel_mass:Double = 5*1000
  def mass:Double = _payload + _fuel_mass
  override def fuelMass: Double = _fuel_mass
  override def fuelMass_=(m: Double): Unit = {_fuel_mass = m}

  val points:List[DVec] = List(
    DVec(-12.0, -8.0),
    DVec(-12.0, 0.0),
    DVec(-16.0, 0.0),
    DVec(-16.0, -4.0),
    DVec(-44.0, -4.0),
    DVec(-44.0, 8.0),
    DVec(-16.0, 8.0),
    DVec(-16.0, 4.0),
    DVec(-12.0, 4.0),
    DVec(12.0, 4.0),
    DVec(16.0, 4.0),
    DVec(16.0, 8.0),
    DVec(44.0, 8.0),
    DVec(44.0, -4.0),
    DVec(16.0, -4.0),
    DVec(16.0, 0.0),
    DVec(12.0, 0.0),
    DVec(12.0, -8.0),
    DVec(4.0, -16.0),
    DVec(-4.0, -16.0)
  )

  val convex_parts = List(
    PolygonShape(List(DVec(-44.0, -4.0), DVec(-16.0, -4.0), DVec(-16.0, 8.0), DVec(-44.0, 8.0)), Nil),
    PolygonShape(List(DVec(-16.0, 0.0), DVec(16.0, 0.0), DVec(16.0, 4.0), DVec(-16.0, 4.0)), Nil),
    PolygonShape(List(DVec(16.0, 8.0), DVec(16.0, -4.0), DVec(44.0, -4.0), DVec(44.0, 8.0)), Nil),
    PolygonShape(List(DVec(-12.0, -8.0), DVec(12.0, -8.0), DVec(12.0, 0.0), DVec(-12.0, 0.0)), Nil),
    PolygonShape(List(DVec(-12.0, -8.0), DVec(-4.0, -16.0), DVec(-4.0, -8.0)), Nil),
    PolygonShape(List(DVec(-4.0, -16.0), DVec(4.0, -16.0), DVec(4.0, -8.0), DVec(-4.0, -8.0)), Nil),
    PolygonShape(List(DVec(4.0, -16.0), DVec(12.0, -8.0), DVec(4.0, -8.0)), Nil)
  )

  convex_parts.foreach(p => {
    println(s"PolygonShape(List(${p.points.map(p => s"DVec(${p.x}, ${p.y})").mkString(", ")}), Nil),")
  })
  println("==========================")

  val wreck_parts = List(
    PolygonShape(List(DVec(-44.0, 8.0), DVec(-44.0, -4.0), DVec(-36.0, -4.0)), Nil),
    PolygonShape(List(DVec(-44.0, 8.0), DVec(-36.0, -4.0), DVec(-32.0, -4.0), DVec(-32.0, 8.0)), Nil),
    PolygonShape(List(DVec(-32.0, -4.0), DVec(-24.0, -4.0), DVec(-28.0, 8.0), DVec(-32.0, 8.0)), Nil),
    PolygonShape(List(DVec(-28.0, 8.0), DVec(-24.0, -4.0), DVec(-24.0, 8.0)), Nil),
    PolygonShape(List(DVec(-24.0, -4.0), DVec(-20.0, -4.0), DVec(-16.0, 8.0), DVec(-24.0, 8.0)), Nil),
    PolygonShape(List(DVec(-16.0, 4.0), DVec(-16.0, 0.0), DVec(-4.0, 0.0), DVec(-4.0, 4.0)), Nil),
    PolygonShape(List(DVec(-4.0, 0.0), DVec(4.0, 0.0), DVec(4.0, 4.0), DVec(-4.0, 4.0)), Nil),
    PolygonShape(List(DVec(4.0, 0.0), DVec(16.0, 0.0), DVec(16.0, 4.0), DVec(4.0, 4.0)), Nil),
    PolygonShape(List(DVec(-12.0, 0.0), DVec(-12.0, -8.0), DVec(-4.0, -8.0), DVec(-4.0, 0.0)), Nil),
    PolygonShape(List(DVec(-12.0, -8.0), DVec(-4.0, -16.0), DVec(4.0, -16.0), DVec(0.0, -8.0)), Nil),
    PolygonShape(List(DVec(-4.0, 0.0), DVec(-4.0, -8.0), DVec(8.0, -8.0), DVec(12.0, 0.0)), Nil),
    PolygonShape(List(DVec(0.0, -8.0), DVec(4.0, -16.0), DVec(12.0, -8.0)), Nil),
    PolygonShape(List(DVec(8.0, -8.0), DVec(12.0, -8.0), DVec(12.0, 0.0)), Nil),
    PolygonShape(List(DVec(16.0, 8.0), DVec(16.0, -4.0), DVec(24.0, -4.0), DVec(28.0, 8.0)), Nil),
    PolygonShape(List(DVec(24.0, -4.0), DVec(32.0, -4.0), DVec(36.0, 8.0), DVec(28.0, 8.0)), Nil),
    PolygonShape(List(DVec(32.0, -4.0), DVec(40.0, -4.0), DVec(36.0, 8.0)), Nil),
    PolygonShape(List(DVec(36.0, 8.0), DVec(40.0, -4.0), DVec(40.0, 8.0)), Nil),
    PolygonShape(List(DVec(40.0, -4.0), DVec(44.0, -4.0), DVec(44.0, 8.0), DVec(40.0, 8.0)), Nil),
    PolygonShape(List(DVec(-20.0, -4.0), DVec(-16.0, -4.0), DVec(-16.0, 8.0)), Nil)
  )

  wreck_parts.foreach(p => {
    println(s"PolygonShape(List(${p.points.map(p => s"DVec(${p.x}, ${p.y})").mkString(", ")}), Nil),")
  })

  println("==========================")

  val docking_points:List[DockingPoints] = Nil

  val two = new Engine("2", position = DVec(0.0, -16.0), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val eight = new Engine("8", position = DVec(0.0, 4.0), force_dir = DVec(0.0, -1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val one = new Engine("1", position = DVec(-38.0, -4.0), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val three = new Engine("3", position = DVec(38.0, -4.0), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val four = new Engine("4", position = DVec(-44.0, 0.0), force_dir = DVec(1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val six = new Engine("6", position = DVec(44.0, 0.0), force_dir = DVec(-1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)

  val engines = List(two, eight, one, three, four, six)

  val engines_mapping = Map(
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three,
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six
  )

  def preserveVelocity(vel:DVec) {}
  def preserveAngularVelocity(ang_vel_deg: Double) {}
}
