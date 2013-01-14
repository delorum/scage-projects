package net.scageprojects.liftme

import net.scage.{ScreenApp, Screen, ScageScreenApp}
import net.scage.support.Vec
import net.scage.ScageLib._
import net.scage.handlers.controller2.MultiController

object ElevatorConstants {
  val floor_width = 40
  val floor_height = 50
  val elevator_speed = 1f
  val elevator_door_speed = 0.01f
}

import ElevatorConstants._

object LiftMe extends ScreenApp("Lift Me!", 800, 600) with MultiController {
  val elevator1 = new Elevator(windowCenter + Vec(-floor_width / 2, floor_height * 9 / 2), 9, this)
}

class Building(val left_down_corner: Vec, val num_floors: Int, val num_lifts: Int, screen: Screen) {

}

/**
 * Нумерация этажей: от 0 до num_floors-1
 *
 * @param left_up_corner
 * @param num_floors
 * @param screen
 */
class Elevator(val left_up_corner: Vec, val num_floors: Int, screen: Screen with MultiController) {
  val elevator_height = num_floors * floor_height
  val door_width = floor_width - 14

  private var pos           = posForFloor(num_floors-1)

  private var door_pos = 1f   // 1 - open door, 0 - close door
  private def drawDoor() {
    door_pos match {
      case 0                   =>
        drawLine(pos + Vec(0, (-floor_height + 7) / 2),
                 pos + Vec(0, ( floor_height - 7) / 2))
      case x if x > 0 && x < 1 =>
        drawLine(pos + Vec(-door_pos*door_width/2, (-floor_height + 7) / 2),
                 pos + Vec(-door_pos*door_width/2, ( floor_height - 7) / 2))
        drawLine(pos + Vec( door_pos*door_width/2, (-floor_height + 7) / 2),
                 pos + Vec( door_pos*door_width/2, ( floor_height - 7) / 2))
      case 1                   =>
      case _                   =>
    }
  }

  private val render_func = screen.render {
    currentColor = WHITE
    drawRect(left_up_corner, floor_width, elevator_height)

    val floor_lines = (1 until num_floors).map {
      x => List(Vec(left_up_corner.x, left_up_corner.y - (num_floors - x) * floor_height), Vec(left_up_corner.x + floor_width, left_up_corner.y - (num_floors - x) * floor_height))
    }.flatten
    drawGroupedLines(floor_lines)
    drawPolygon(elevator_area)

    drawRectCentered(pos, floor_width - 7, floor_height - 7)
    drawDoor()

    if(moving_step.notZero) {
      drawRectCentered(target_floor_pos, floor_width+3, floor_height+3, RED)
    }
  }

  val elevator_area = List(left_up_corner,
                           left_up_corner + Vec(floor_width, 0),
                           left_up_corner + Vec(floor_width, -elevator_height),
                           left_up_corner + Vec(0,           -elevator_height))
  private val left_mouse = screen.leftMouseOnArea(elevator_area, onBtnDown = mouse => {
    moveToFloor(floorForCoord(mouse))
  })

  def posForFloor(floor:Int):Vec = {
    Vec(left_up_corner.x + floor_width/2, left_up_corner.y - (num_floors - floor) * floor_height + floor_height/2)
  }

  def floorForCoord(coord:Vec):Int = {
    (coord.y - left_up_corner.y + elevator_height) / floor_height match {
      case x if x <= 0            => 0
      case x if x >= num_floors-1 => num_floors-1
      case x                      => x.toInt
    }
  }

  private var target_floor_pos = pos
  private var moving_step      = Vec.zero
  def moveToFloor(floor: Int) {
    if(moving_step.notZero) {
      val new_target_floor_pos = posForFloor(floor)
      if((moving_step.y <  0 && new_target_floor_pos.y < pos.y && new_target_floor_pos.y > target_floor_pos.y) ||
         (moving_step.y >= 0 && new_target_floor_pos.y > pos.y && new_target_floor_pos.y < target_floor_pos.y)) {
        target_floor_pos = new_target_floor_pos
      }
    } else {
      target_floor_pos = posForFloor(floor)
      moving_step = Vec(0, math.signum(target_floor_pos.y - pos.y)*elevator_speed)
      screen.action {
        if(removed) screen.deleteSelf()
        else {
          door_pos -= elevator_door_speed
          if (door_pos <= 0) {
            door_pos = 0
            screen.deleteSelf()
            screen.action {
              if(removed) screen.deleteSelf()
              else {
                pos += moving_step
                if((moving_step.y <  0  && pos.y < target_floor_pos.y) ||
                   (moving_step.y >= 0 && pos.y >= target_floor_pos.y)) {
                  pos = target_floor_pos
                  screen.deleteSelf()
                  screen.action {
                    if(removed) screen.deleteSelf()
                    else {
                      door_pos += elevator_door_speed
                      if (door_pos >= 1) {
                        door_pos = 1
                        screen.deleteSelf()
                        moving_step = Vec.zero
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  private var removed = false
  def remove() {
    removed = true
    screen.delOperations(render_func, left_mouse)
  }

  screen.clear {
    remove()
    screen.deleteSelf()
  }
}
