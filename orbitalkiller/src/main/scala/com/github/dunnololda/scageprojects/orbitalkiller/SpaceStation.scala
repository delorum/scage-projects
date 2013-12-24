package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

class SpaceStation(
             index:String,
             init_coord:Vec,
             init_velocity:Vec = Vec.zero,
             init_rotation:Float = 0f
             ) extends PolygonShip(index, init_coord, init_velocity, init_rotation) {
  val points:List[Vec] = List(
    Vec(-50.0, 50.0),
    Vec(-30.0, 50.0),
    Vec(-30.0, 30.0),
    Vec(-10.0, 10.0),
    Vec(-10.0, -10.0),
    Vec(-30.0, -30.0),
    Vec(-30.0, -50.0),
    Vec(-50.0, -50.0),
    Vec(-50.0, -110.0),
    Vec(-30.0, -110.0),
    Vec(-30.0, -70.0),
    Vec(30.0, -70.0),
    Vec(30.0, -110.0),
    Vec(50.0, -110.0),
    Vec(50.0, -50.0),
    Vec(30.0, -50.0),
    Vec(30.0, -30.0),
    Vec(10.0, -10.0),
    Vec(10.0, 10.0),
    Vec(30.0, 30.0),
    Vec(30.0, 50.0),
    Vec(50.0, 50.0),
    Vec(50.0, 110.0),
    Vec(30.0, 110.0),
    Vec(30.0, 90.0),
    Vec(30.0, 70.0),
    Vec(10.0, 70.0),
    Vec(-10.0, 70.0),
    Vec(-30.0, 70.0),
    Vec(-30.0, 90.0),
    Vec(-30.0, 110.0),
    Vec(-50.0, 110.0)
  )

  val four  = Engine(position = Vec(-10.0, 0.0),    force_dir = Vec(1.0, 0.0),  max_power = 10, power_step = 1, this)
  val six   = Engine(position = Vec(10.0, 0.0),     force_dir = Vec(-1.0, 0.0), max_power = 10, power_step = 1, this)
  val seven = Engine(position = Vec(-40.0, 110.0),  force_dir = Vec(0.0, -1.0), max_power = 10, power_step = 1, this)
  val nine  = Engine(position = Vec(40.0, 110.0),   force_dir = Vec(0.0, -1.0), max_power = 10, power_step = 1, this)
  val one   = Engine(position = Vec(-40.0, -110.0), force_dir = Vec(0.0, 1.0),  max_power = 10, power_step = 1, this)
  val three = Engine(position = Vec(40.0, -110.0),  force_dir = Vec(0.0, 1.0),  max_power = 10, power_step = 1, this)

  val engines = List(four, six, seven, nine, one, three)

  val engines_mapping = Map(
    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six,
    KEY_NUMPAD7 -> seven,
    KEY_NUMPAD9 -> nine,
    KEY_NUMPAD1 -> one,
    KEY_NUMPAD3 -> three
  )

  def enterOrbit() {

  }

  def mass: Float = 1


  def preserveAngularVelocity(ang_vel_deg: Float) {

  }

  render {
    openglLocalTransform {
      openglMove(coord)
      openglRotateDeg(rotation)
      drawSlidingLines(points :+ points.head, WHITE)

      engines.foreach {
        case e =>
          e.force_dir match {
            case Vec(0, -1) => drawEngine(e, e.position + Vec(0, 2.5f),  10, 5,  is_vertical = false)
            case Vec(0, 1)  => drawEngine(e, e.position + Vec(0, -2.5f), 10, 5,  is_vertical = false)
            case Vec(-1, 0) => drawEngine(e, e.position + Vec(2.5f, 0),  5,  10, is_vertical = true)
            case Vec(1, 0)  => drawEngine(e, e.position + Vec(-2.5f, 0), 5,  10, is_vertical = true)
            case _ =>
          }
      }
    }

    drawFilledCircle(coord, 2, GREEN)                             // mass center
    drawLine(coord, coord + linearVelocity.n*100, CYAN)           // current velocity
    drawLine(coord, coord + (earth.coord - coord).n*100, YELLOW)    // direction to sun
    drawLine(coord, coord + (moon.coord - coord).n*100, GREEN)   // direction to earth
  }
}
