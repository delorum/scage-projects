package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

class SpaceStation2(
             index:String,
             init_coord:DVec,
             init_velocity:DVec = DVec.dzero,
             init_rotation:Double = 0.0
             ) extends PolygonShip(index, init_coord, init_velocity, init_rotation) {
  def mass:Double = _payload + _fuel_mass
  private val _payload:Double = 50*1000
  private var _fuel_mass:Double = 50*1000
  override def fuelMass: Double = _fuel_mass

  override def fuelMass_=(m: Double): Unit = {
    _fuel_mass = m
  }


  val points:List[DVec] = List(
    DVec(-90.0, -10.0),
    DVec(-130.0, -10.0),
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

  val four  = Engine("4", position = Vec(-130.0, 0.0),   force_dir = Vec(1.0, 0.0),  max_power = 10, default_power_percent = 1, this)
  val six   = Engine("6", position = Vec(130.0, 0.0),    force_dir = Vec(-1.0, 0.0), max_power = 10, default_power_percent = 1, this)
  val eight = Engine("8", position = Vec(0.0, 30.0),     force_dir = Vec(0.0, -1.0), max_power = 10, default_power_percent = 1, this)
  val two   = Engine("2", position = Vec(0.0, -30.0),    force_dir = Vec(0.0, 1.0),  max_power = 10, default_power_percent = 1, this)
  val one   = Engine("1", position = Vec(-120.0, -10.0), force_dir = Vec(0.0, 1.0),  max_power = 10, default_power_percent = 1, this)
  val three = Engine("3", position = Vec(120.0, -10.0),  force_dir = Vec(0.0, 1.0),  max_power = 10, default_power_percent = 1, this)

  val engines = List(four, six, eight, two, one, three)

  val engines_mapping = Map(
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD8 -> eight,
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

  val draw_points = points :+ points.head

  def preserveVelocity(vel:DVec) {

  }

  def preserveAngularVelocity(ang_vel_deg: Double) {

  }

  render {
    /*if(renderingEnabled) {*/
      if(!drawMapMode) {
        openglLocalTransform {
          openglMove(coord - base)
          drawFilledCircle(DVec.zero, 2, GREEN)                                // mass center
          if(OrbitalKiller.globalScale >= 0.2) {
            drawArrow(DVec.zero, relativeLinearVelocity.n * 100, CYAN) // current velocity
          }

          openglRotateDeg(rotation)
          drawSlidingLines(draw_points, WHITE)

          engines.foreach {
            case e =>
              e.force_dir match {
                case DVec(0, -1) => drawEngine(e, e.position + DVec(0, 2.5),  10, 5,  is_vertical = false)
                case DVec(0, 1)  => drawEngine(e, e.position + DVec(0, -2.5), 10, 5,  is_vertical = false)
                case DVec(-1, 0) => drawEngine(e, e.position + DVec(2.5, 0),  5,  10, is_vertical = true)
                case DVec(1, 0)  => drawEngine(e, e.position + DVec(-2.5,0), 5,  10, is_vertical = true)
                case _ =>
              }
          }
        }
      }
    /*}*/
  }
}
