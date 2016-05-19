package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.{DockingPoints, PolygonShape, Engine, PolygonShip}

class Cargo1(index: Int,
             init_coord: DVec,
             init_velocity: DVec = DVec.dzero,
             init_rotation: Double = 0.0,
             ship_designer:Boolean = false) extends PolygonShip(index, "Приятель", init_coord, init_velocity, init_rotation, ship_designer, true) {

  private val _payload:Double = 5*1000
  private var _fuel_mass:Double = 5*1000
  def mass:Double = _payload + _fuel_mass
  override def fuelMass: Double = _fuel_mass
  override def fuelMass_=(m: Double): Unit = {_fuel_mass = m}

  lazy val engine_size:Double = 0.5

  lazy val points:List[DVec] = List(
    DVec(-2.5, 2.5),
    DVec(-2.5, -1.5),
    DVec(-1.5, -2.5),
    DVec(1.5, -2.5),
    DVec(2.5, -1.5),
    DVec(2.5, 2.5)
  )

  lazy val convex_parts = List(
    PolygonShape(List(DVec(-2.5, -1.5), DVec(-1.5, -2.5), DVec(1.5, -2.5), DVec(2.5, -1.5), DVec(2.5, 2.5), DVec(-2.5, 2.5)), List())
  )

  val wreck_parts = List(
    PolygonShape(List(DVec(-2.5, -1.5), DVec(-1.5, -2.5), DVec(-0.5, -0.5)), List()),
    PolygonShape(List(DVec(-1.5, -2.5), DVec(0.5, -2.5), DVec(0.5, 0.5), DVec(-0.5, -0.5)), List()),
    PolygonShape(List(DVec(0.5, -2.5), DVec(1.5, -2.5), DVec(2.5, -1.5), DVec(0.5, 0.5)), List()),
    PolygonShape(List(DVec(0.5, 0.5), DVec(2.5, -1.5), DVec(2.5, 1.5), DVec(-0.5, 2.5)), List()),
    PolygonShape(List(DVec(-0.5, 2.5), DVec(2.5, 1.5), DVec(2.5, 2.5)), List()),
    PolygonShape(List(DVec(-2.5, -1.5), DVec(-0.5, -0.5), DVec(0.5, 0.5), DVec(-2.5, 0.5)), List()),
    PolygonShape(List(DVec(-2.5, 0.5), DVec(0.5, 0.5), DVec(-0.5, 2.5)), List()),
    PolygonShape(List(DVec(-2.5, 0.5), DVec(-0.5, 2.5), DVec(-2.5, 2.5)), List())
  )

  val docking_points = List(
    new DockingPoints(DVec(1.5, 2.5), DVec(-1.5, 2.5), this, None)
  )

  val engines = List()

  val engines_mapping: Map[Int, Engine] = Map()

  def preserveVelocity(vel:DVec) {}
  def preserveAngularVelocity(ang_vel_deg: Double) {}

  override val is_manned: Boolean = false
}
