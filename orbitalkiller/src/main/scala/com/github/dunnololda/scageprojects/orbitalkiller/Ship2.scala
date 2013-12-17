package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import OrbitalKiller._

class Ship2(
  val index:String,
  init_coord:Vec,
  init_velocity:Vec = Vec.zero,
  init_rotation:Float = 0f
) extends Ship {
  val mass:Float = 1   // mass

  val points:List[Vec] = List(
    Vec(-10.0, 70.74075),
    Vec(10.0, 70.74075),
    Vec(10.0, 10.740753),
    Vec(50.0, -9.259247),
    Vec(10.0, -9.259247),
    Vec(10.0, -69.25925),
    Vec(-10.0, -69.25925),
    Vec(-10.0, -9.259247),
    Vec(-50.0, -9.259247),
    Vec(-10.0, 10.740753),
    Vec(-10.0, 70.74075)
  )

  val eight = Engine(Vec(0.0, 70.74075), force_dir = Vec(0.0, -1.0), max_power = 10, this)
  val two = Engine(position = Vec(0.0, -69.25925), force_dir = Vec(0.0, 1.0), max_power = 10, this)

  val four = Engine(position = Vec(-40.0, -9.259247), force_dir = Vec(0.0, 1.0), max_power = 10, this)
  val six =  Engine(position = Vec(40.0, -9.259247), force_dir = Vec(0.0, 1.0), max_power = 10, this)

  val engines = List(two, four, six, eight)

  val engines_mapping = Map(
    KEY_NUMPAD2 -> two,
    KEY_NUMPAD8 -> eight,

    KEY_NUMPAD4 -> four,
    KEY_NUMPAD6 -> six
  )

  def currentState:BodyState = currentBodyState(index).getOrElse(
    BodyState(
      index,
      mass,
      acc = Vec.zero,
      vel = init_velocity,
      coord = init_coord,
      ang_acc = 0f,
      ang_vel = 0f,
      ang = init_rotation,
      shape = PolygonShape(points.distinct),
      is_static = false)
  )

  def rotateRight() {
    activateOnlyOneEngine(four)
  }

  def smallRotateRight() {
    activateOnlyOneEngine(four)
  }

  def rotateLeft() {
    activateOnlyOneEngine(six)
  }

  def smallRotateLeft() {
    activateOnlyOneEngine(six)
  }

  private def drawEngine(e:Engine, center:Vec, width:Float, height:Float, is_vertical:Boolean) {
    drawRectCentered(center, width, height, color = engineColor(e))
    if(e.active && e.power > 0) {
      if(is_vertical) {
        drawFilledRectCentered(center, width, engineActiveSize(e), color = engineColor(e))
      } else {
        drawFilledRectCentered(center, engineActiveSize(e), height, color = engineColor(e))
      }
      if(globalScale > 2) print(f"${e.power/e.max_power*100f}%.0f% : ${e.worktimeTacts}", center, size = max_font_size/globalScale)
      if(isSelectedEngine(e)) drawRectCentered(center, width+2, height+2, color = engineColor(e))
    }
  }

  action {
    flightMode match {
      case 1 =>
      case 2 => // запрет вращения
        if(math.abs(angularVelocity*60*base_dt) < 0.01f) flightMode = 1
        else preserveAngularVelocity(0)
      case 3 => // ориентация по осям
        if(math.abs(rotation*60*base_dt) < 0.1f) flightMode = 2
        else preserveAngle(0)
      case 4 => // ориентация по траектории
        val angle = linearVelocity.deg(Vec(0,1).rotateDeg(rotation)) * math.signum(linearVelocity.perpendicular * Vec(0,1).rotateDeg(rotation))
        if(math.abs(angle) < 0.01f) flightMode = 2
        else {

        }
      case _ =>
    }
  }

  render {
    openglLocalTransform {
      openglMove(coord)
      openglRotateDeg(rotation)
      drawSlidingLines(points, WHITE)

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
    drawLine(coord, coord + (sun.coord - coord).n*100, YELLOW)    // direction to sun
    drawLine(coord, coord + (earth.coord - coord).n*100, GREEN)   // direction to earth
  }
}
