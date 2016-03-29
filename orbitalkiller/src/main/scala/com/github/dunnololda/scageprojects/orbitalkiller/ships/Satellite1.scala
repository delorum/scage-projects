package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
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
    DVec(-3.0, -2.0),
    DVec(-3.0, 0.0),
    DVec(-4.0, 0.0),
    DVec(-4.0, -1.0),
    DVec(-11.0, -1.0),
    DVec(-11.0, 2.0),
    DVec(-4.0, 2.0),
    DVec(-4.0, 1.0),
    DVec(-3.0, 1.0),
    DVec(3.0, 1.0),
    DVec(4.0, 1.0),
    DVec(4.0, 2.0),
    DVec(11.0, 2.0),
    DVec(11.0, -1.0),
    DVec(4.0, -1.0),
    DVec(4.0, 0.0),
    DVec(3.0, 0.0),
    DVec(3.0, -2.0),
    DVec(1.0, -4.0),
    DVec(-1.0, -4.0)
  )

  val draw_points = points :+ points.head

  val convex_parts = List(
    PolygonShape(List(DVec(-11.0, -1.0), DVec(-4.0, -1.0), DVec(-4.0, 2.0), DVec(-11.0, 2.0)), Nil),
    PolygonShape(List(DVec(-4.0, 0.0), DVec(4.0, 0.0), DVec(4.0, 1.0), DVec(-4.0, 1.0)), Nil),
    PolygonShape(List(DVec(4.0, 2.0), DVec(4.0, -1.0), DVec(11.0, -1.0), DVec(11.0, 2.0)), Nil),
    PolygonShape(List(DVec(-3.0, -2.0), DVec(3.0, -2.0), DVec(3.0, 0.0), DVec(-3.0, 0.0)), Nil),
    PolygonShape(List(DVec(-3.0, -2.0), DVec(-1.0, -4.0), DVec(-1.0, -2.0)), Nil),
    PolygonShape(List(DVec(-1.0, -4.0), DVec(1.0, -4.0), DVec(1.0, -2.0), DVec(-1.0, -2.0)), Nil),
    PolygonShape(List(DVec(1.0, -4.0), DVec(3.0, -2.0), DVec(1.0, -2.0)), Nil)
  )

  val wreck_parts = List(
    PolygonShape(List(DVec(-11.0, 2.0), DVec(-11.0, -1.0), DVec(-9.0, -1.0)), Nil),
    PolygonShape(List(DVec(-11.0, 2.0), DVec(-9.0, -1.0), DVec(-8.0, -1.0), DVec(-8.0, 2.0)), Nil),
    PolygonShape(List(DVec(-8.0, -1.0), DVec(-6.0, -1.0), DVec(-7.0, 2.0), DVec(-8.0, 2.0)), Nil),
    PolygonShape(List(DVec(-7.0, 2.0), DVec(-6.0, -1.0), DVec(-6.0, 2.0)), Nil),
    PolygonShape(List(DVec(-6.0, -1.0), DVec(-5.0, -1.0), DVec(-4.0, 2.0), DVec(-6.0, 2.0)), Nil),
    PolygonShape(List(DVec(-4.0, 1.0), DVec(-4.0, 0.0), DVec(-1.0, 0.0), DVec(-1.0, 1.0)), Nil),
    PolygonShape(List(DVec(-1.0, 0.0), DVec(1.0, 0.0), DVec(1.0, 1.0), DVec(-1.0, 1.0)), Nil),
    PolygonShape(List(DVec(1.0, 0.0), DVec(4.0, 0.0), DVec(4.0, 1.0), DVec(1.0, 1.0)), Nil),
    PolygonShape(List(DVec(-3.0, 0.0), DVec(-3.0, -2.0), DVec(-1.0, -2.0), DVec(-1.0, 0.0)), Nil),
    PolygonShape(List(DVec(-3.0, -2.0), DVec(-1.0, -4.0), DVec(1.0, -4.0), DVec(0.0, -2.0)), Nil),
    PolygonShape(List(DVec(-1.0, 0.0), DVec(-1.0, -2.0), DVec(2.0, -2.0), DVec(3.0, 0.0)), Nil),
    PolygonShape(List(DVec(0.0, -2.0), DVec(1.0, -4.0), DVec(3.0, -2.0)), Nil),
    PolygonShape(List(DVec(2.0, -2.0), DVec(3.0, -2.0), DVec(3.0, 0.0)), Nil),
    PolygonShape(List(DVec(4.0, 2.0), DVec(4.0, -1.0), DVec(6.0, -1.0), DVec(7.0, 2.0)), Nil),
    PolygonShape(List(DVec(6.0, -1.0), DVec(8.0, -1.0), DVec(9.0, 2.0), DVec(7.0, 2.0)), Nil),
    PolygonShape(List(DVec(8.0, -1.0), DVec(10.0, -1.0), DVec(9.0, 2.0)), Nil),
    PolygonShape(List(DVec(9.0, 2.0), DVec(10.0, -1.0), DVec(10.0, 2.0)), Nil),
    PolygonShape(List(DVec(10.0, -1.0), DVec(11.0, -1.0), DVec(11.0, 2.0), DVec(10.0, 2.0)), Nil),
    PolygonShape(List(DVec(-5.0, -1.0), DVec(-4.0, -1.0), DVec(-4.0, 2.0)), Nil)
  )

  val docking_points:List[DockingPoints] = Nil

  val two = new Engine("2", position = DVec(0.0, -4.0), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val eight = new Engine("8", position = DVec(0.0, 1.0), force_dir = DVec(0.0, -1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val one = new Engine("1", position = DVec(-9.5, -1.0), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val three = new Engine("3", position = DVec(9.5, -1.0), force_dir = DVec(0.0, 1.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val four = new Engine("4", position = DVec(-11.0, 0.0), force_dir = DVec(1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)
  val six = new Engine("6", position = DVec(11.0, 0.0), force_dir = DVec(-1.0, 0.0), max_power = 10, default_power_percent = 1, fuel_consumption_per_sec_at_full_power = 4, this)

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

  render {
    /*if(renderingEnabled) {*/
    if(!drawMapMode && coord.dist2(ship.coord) < 100000*100000) {
      openglLocalTransform {
        openglMove(coord - base)
        drawFilledCircle(DVec.zero, 0.3, GREEN)                                // mass center
        if(OrbitalKiller.globalScale >= 0.8) {
          drawArrow(DVec.zero, relativeLinearVelocity.n * 20, CYAN) // current velocity
        }

        openglRotateDeg(rotation)
        drawSlidingLines(draw_points, WHITE)

        engines.foreach {
          case e => drawEngine(e, 1)
        }
      }
    }
    /*}*/
  }
}
