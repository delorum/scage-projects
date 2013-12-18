package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._

abstract class PolygonShip(
  val index:String,
  init_coord:Vec,
  init_velocity:Vec = Vec.zero,
  init_rotation:Float = 0f) extends Ship {
  def points:List[Vec]
  def engines:List[Engine]
  def engines_mapping:Map[Int, Engine]

  val mass:Float = 1   // mass

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
      shape = PolygonShape(points),
      is_static = false)
  )

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
        val angle = linearVelocity.mydeg(Vec(0,1))
        if(math.abs((rotation - angle)*60*base_dt) < 0.1f) flightMode = 2
        else preserveAngle(angle)
      case _ =>
    }
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
