package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.{Engine, OrbitalKiller, PolygonShape, PolygonShip}

class SpaceStation2(
             index:Int,
             init_coord:DVec,
             init_velocity:DVec = DVec.dzero,
             init_rotation:Double = 0.0
             ) extends PolygonShip(index, "Станция Огонек", init_coord, init_velocity, init_rotation) {
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

  override val convex_parts = List(
    PolygonShape(List(DVec(-130.0, -10.0), DVec(-130.0, 10.0), DVec(-90.0, 10.0), DVec(-90.0, -10.0)).reverse, Nil),
    PolygonShape(List(DVec(-90.0, -10.0), DVec(-90.0, 10.0), DVec(-50.0, 30.0), DVec(-50.0, -30.0)).reverse, Nil),
    PolygonShape(List(DVec(-50.0, -30.0), DVec(-50.0, 30.0), DVec(50.0, 30.0), DVec(50.0, -30.0)).reverse, Nil),
    PolygonShape(List(DVec(50.0, -30.0), DVec(50.0, 30.0), DVec(90.0, 10.0), DVec(90.0, -10.0)).reverse, Nil),
    PolygonShape(List(DVec(90.0, -10.0), DVec(90.0, 10.0), DVec(130.0, 10.0), DVec(130.0, -10.0)).reverse, Nil)
  )

  override val wreck_parts = convex_parts

  val four  = new Engine("4", Vec(-130.0, 0.0),   Vec(1.0, 0.0),  10, 1, 4, this)
  val six   = new Engine("6", Vec(130.0, 0.0),    Vec(-1.0, 0.0), 10, 1, 4, this)
  val eight = new Engine("8", Vec(0.0, 30.0),     Vec(0.0, -1.0), 10, 1, 4, this)
  val two   = new Engine("2", Vec(0.0, -30.0),    Vec(0.0, 1.0),  10, 1, 4, this)
  val one   = new Engine("1", Vec(-120.0, -10.0), Vec(0.0, 1.0),  10, 1, 4, this)
  val three = new Engine("3", Vec(120.0, -10.0),  Vec(0.0, 1.0),  10, 1, 4, this)

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
      if(!drawMapMode && coord.dist2(ship.coord) < 100000*100000) {
        openglLocalTransform {
          openglMove(coord - base)
          drawFilledCircle(DVec.zero, 2, GREEN)                                // mass center
          if(OrbitalKiller.globalScale >= 0.2) {
            drawArrow(DVec.zero, relativeLinearVelocity.n * 100, CYAN) // current velocity
          }

          openglRotateDeg(rotation)
          drawSlidingLines(draw_points, WHITE)

          engines.foreach {
            case e => drawEngine(e, 10)
          }
        }
      }
    /*}*/
  }
}
