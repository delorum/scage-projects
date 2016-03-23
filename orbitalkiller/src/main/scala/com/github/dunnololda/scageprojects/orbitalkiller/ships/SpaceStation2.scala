package com.github.dunnololda.scageprojects.orbitalkiller.ships

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller._

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
    PolygonShape(List(DVec(-130.0, -10.0), DVec(-90.0, -10.0), DVec(-90.0, 10.0), DVec(-130.0, 10.0)), Nil),
    PolygonShape(List(DVec(-90.0, -10.0), DVec(-50.0, -30.0), DVec(-50.0, 30.0), DVec(-90.0, 10.0)), Nil),
    PolygonShape(List(DVec(-50.0, -30.0), DVec(50.0, -30.0), DVec(50.0, 30.0), DVec(-50.0, 30.0)), Nil),
    PolygonShape(List(DVec(50.0, -30.0), DVec(90.0, -10.0), DVec(90.0, 10.0), DVec(50.0, 30.0)), Nil),
    PolygonShape(List(DVec(90.0, -10.0), DVec(130.0, -10.0), DVec(130.0, 10.0), DVec(90.0, 10.0)), Nil)
  )

  override val wreck_parts = List(
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

  override val docking_points = List(new DockingPoints(DVec(-130.0, 1.5), DVec(-130.0, -1.5), this))

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
          if(OrbitalKiller.globalScale >= 0.8) {
            drawArrow(DVec.zero, relativeLinearVelocity.n * 100, CYAN) // current velocity
          }

          openglRotateDeg(rotation)
          drawSlidingLines(draw_points, WHITE)

          if (OrbitalKiller.globalScale >= 0.8) {
            if(InterfaceHolder.dockingSwitcher.dockingEnabled) {
              docking_points.foreach(dp => {
                if(canDockWithNearestShipUsingDockPoints(dp)) {
                  drawFilledCircle(dp.p1, 0.3, colorIfAliveOrRed(GREEN))
                  drawFilledCircle(dp.p2, 0.3, colorIfAliveOrRed(GREEN))
                } else {
                  val (p1_on_the_right_way, p2_on_the_right_way) = OrbitalKiller.ship.docking_points.headOption.map(_.pointsOnTheRightWay(dp)).getOrElse((false, false))

                  val c1 = if(p1_on_the_right_way) GREEN else RED
                  val c2 = if(p2_on_the_right_way) GREEN else RED

                  val v1 = (dp.p1-dp.p2).n
                  val v2 = v1.perpendicular

                  drawLine(dp.p1, dp.p1+v2*100, colorIfAliveOrRed(c1))
                  drawLine(dp.p2, dp.p2+v2*100, colorIfAliveOrRed(c2))

                  drawFilledCircle(dp.p1, 0.3, colorIfAliveOrRed(RED))
                  drawCircle(dp.p1, 1, colorIfAliveOrRed(RED))
                  drawFilledCircle(dp.p2, 0.3, colorIfAliveOrRed(RED))
                  drawCircle(dp.p2, 1, colorIfAliveOrRed(RED))
                }
              })
            }
          }

          engines.foreach {
            case e => drawEngine(e, 10)
          }
        }
      }
    /*}*/
  }
}
