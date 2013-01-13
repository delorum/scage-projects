package net.scageprojects.liftme

import net.scage.{Screen, ScageScreenApp}
import net.scage.support.Vec
import net.scage.ScageLib._

object ElevatorConstants {
  val floor_width = 40
  val floor_height = 50
  val elevator_speed = 1f
}

import ElevatorConstants._

object LiftMe extends ScageScreenApp("Lift Me!", 800, 600) {
  val elevator1 = new Elevator(windowCenter + Vec(-floor_width / 2, floor_height * 9 / 2), 9, this)
  key(KEY_1, onKeyDown = {elevator1.moveToFloor(0)})
  key(KEY_2, onKeyDown = {elevator1.moveToFloor(1)})
  key(KEY_3, onKeyDown = {elevator1.moveToFloor(2)})
  key(KEY_4, onKeyDown = {elevator1.moveToFloor(3)})
  key(KEY_5, onKeyDown = {elevator1.moveToFloor(4)})
  key(KEY_6, onKeyDown = {elevator1.moveToFloor(5)})
  key(KEY_7, onKeyDown = {elevator1.moveToFloor(6)})
  key(KEY_8, onKeyDown = {elevator1.moveToFloor(7)})
  key(KEY_9, onKeyDown = {elevator1.moveToFloor(8)})
}

class Building(val left_down_corner: Vec, val num_floors: Int, val num_lifts: Int, screen: Screen) {

}

class Elevator(val left_up_corner: Vec, val num_floors: Int, screen: Screen) {

  val elevator_height = num_floors * floor_height
  private var pos = posForFloor(num_floors-1)
  private var is_moving = false

  private val render_func = screen.render {
    drawRect(left_up_corner, floor_width, elevator_height)

    val floor_lines = (1 until num_floors).map {
      x => List(Vec(left_up_corner.x, left_up_corner.y - (num_floors - x) * floor_height), Vec(left_up_corner.x + floor_width, left_up_corner.y - (num_floors - x) * floor_height))
    }.flatten
    drawGroupedLines(floor_lines)

    drawRectCentered(pos, floor_width - 7, floor_height - 7)
    drawLine(pos + Vec(0, (-floor_height + 7) / 2), pos + Vec(0, (floor_height - 7) / 2))
  }

  def posForFloor(floor:Int):Vec = {
    Vec(left_up_corner.x + floor_width/2, left_up_corner.y - (num_floors - floor) * floor_height + floor_height/2)
  }

  def moveToFloor(floor: Int) {
    if (!is_moving) {
      is_moving = true
      val target_floor_pos = posForFloor(floor)
      val moving_step = Vec(0, math.signum(target_floor_pos.y - pos.y)*elevator_speed)
      screen.action {
        if(removed) screen.deleteSelf()
        else {
          pos += moving_step
          if((moving_step.y < 0 && pos.y < target_floor_pos.y) || (moving_step.y >= 0 && pos.y >= target_floor_pos.y)) {
            pos = target_floor_pos
            is_moving = false
            screen.deleteSelf()
          }
        }
      }
    }
  }

  private var removed = false
  def remove() {
    removed = true
    screen.delOperation(render_func)
  }

  screen.clear {
    remove()
    screen.deleteSelf()
  }
}
