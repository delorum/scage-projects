package net.scageprojects.liftdriver

import net.scage.{ScreenApp, Screen}
import net.scage.support.Vec
import net.scage.ScageLib._
import net.scage.handlers.controller2.MultiController
import collection.mutable.ArrayBuffer

object ElevatorConstants {
  val floor_width = 40
  val floor_height = 50
  val elevator_speed = 1f
  val elevator_door_speed = 0.01f
}

import ElevatorConstants._

// Новая Игра, Демонстрация, Помощь, Выход
// Внизу сбоку - выбор языка, ru и en, по умолчанию en
object MainMenu extends ScreenApp("Lift Driver", 800, 600) with MultiController {
  interface {
    print("New Game",  windowCenter, align = "center")
    print("Demo Mode", windowCenter - Vec(0, 30), align = "center")
    print("Help",      windowCenter - Vec(0, 60), align = "center")
    print("Exit",      windowCenter - Vec(0, 90), align = "center")
    //print("English",   Vec(windowWidth-10, 10),      align = "right")
  }

  private var manual_mode = true
  def manualMode = manual_mode

  val Vec(new_w, new_h) = messageBounds("New Game")
  leftMouseOnRectCentered(windowCenter, new_w, new_h, onBtnDown = m => {
    manual_mode = true
    LiftDriver.run()
  })

  val Vec(demo_w, demo_h) = messageBounds("Demo Mode")
  leftMouseOnRectCentered(windowCenter - Vec(0, 30), demo_w, demo_h, onBtnDown = m => {
    manual_mode = false
    LiftDriver.run()
  })

  val Vec(help_w, help_h) = messageBounds("Help")
  leftMouseOnRectCentered(windowCenter - Vec(0, 60), help_w, help_h, onBtnDown = m => {
    HelpScreen.run()
  })

  val Vec(exit_w, exit_h) = messageBounds("Exit")
  leftMouseOnRectCentered(windowCenter - Vec(0, 90), exit_w, exit_h, onBtnDown = m => stopApp())
}

object HelpScreen extends Screen with MultiController {
  interface {
    print("Navigate elevators to floors with passengers and then\n" +
          "to the floors they want to go (yellow frames).\n" +
          "And try to do it really fast!\n" +
          "Click or press any key to exit", windowCenter, align = "center")
  }

  anykey(onKeyDown = stop())
  leftMouse(onBtnDown = m => stop())
}

object LiftDriver extends Screen with MultiController {
  private var num_issued_passengers  = 0
  private var best_transport_time    = Long.MaxValue
  private var worst_transport_time   = 0l
  private var overall_transport_time = 0l
  private var average_transport_time = 0l
  private var num_transported        = 0

  def passengersAmount = if(MainMenu.manualMode) 20 else 50

  init {
    num_issued_passengers = 0
    best_transport_time   = Long.MaxValue
    worst_transport_time  = 0l
    num_transported       = 0
    overall_transport_time = 0l
    average_transport_time = 0l

    val building1 = new Building(windowCenter + Vec(-floor_width / 2, floor_height * 9 / 2), 9, this)

    if(MainMenu.manualMode) {
      building1.addElevator(8)
      building1.addElevator(4)
      building1.addElevator(4)
    } else {
      building1.addElevator(8)
      building1.addElevator(4)
      building1.addElevator(4)
      building1.addElevator(4)
      building1.addElevator(4)
      building1.addElevator(4)
      new LiftDriverAI(building1, this)
    }

    val action_func = action(1000) {
      if(num_issued_passengers >= passengersAmount) deleteSelf()
      else {
        if(math.random < 0.4) {
          building1.issuePassenger(msecsFromInitWithoutPause)
          num_issued_passengers += 1
        }
      }
    }

    clear {
      delOperationsNoWarn(action_func, currentOperation)
    }

    pause()
  }

  onEventWithArguments("transported") {
    case transported_passengers:Seq[Passenger] =>
      transported_passengers.foreach(p => {
        val transport_time = msecsFromInitWithoutPause - p.start_waiting
        if(transport_time > worst_transport_time) worst_transport_time = transport_time
        if(transport_time < best_transport_time)  best_transport_time = transport_time
        overall_transport_time += transport_time
      })
      num_transported += transported_passengers.length
      average_transport_time = overall_transport_time/num_transported
  }

  keyIgnorePause(KEY_SPACE, onKeyDown = switchPause())
  keyIgnorePause(KEY_F2, onKeyDown = {
    if(onPause || num_transported >= passengersAmount) restart()
  })
  keyIgnorePause(KEY_ESCAPE, onKeyDown = {
    if(onPause || num_transported >= passengersAmount) stop()
  })

  render {
    if(onPause)                                   print("PAUSE. PRESS SPACE. ESC TO EXIT",  windowCenter, YELLOW, align = "center")
    else if(num_transported >= passengersAmount) print("PRESS F2 TO RESTART, ESC TO EXIT", windowCenter, YELLOW, align = "center")
    currentColor = WHITE
    print("Transported:                "+num_transported+"/"+passengersAmount, 20, windowHeight-20)
    if(best_transport_time < Long.MaxValue) {
      print("Best Transport Time:   "+(best_transport_time/1000)+" sec",        20, windowHeight-40)
    } else print("Best Transport Time:   Unknown",                              20, windowHeight-40)
    if(worst_transport_time > 0) {
      print("Worst Transport Time: "+(worst_transport_time/1000)+" sec",        20, windowHeight-60)
    } else print("Worst Transport Time: Unknown",                               20, windowHeight-60)
    if(average_transport_time > 0) {
      print("Average Transport Time: "+(average_transport_time/1000)+" sec",    20, windowHeight-80)
    } else print("Average Transport Time: Unknown",                             20, windowHeight-80)
  }
}

case class Passenger(floor:Int, target_floor:Int, start_waiting:Long)

class Building(val left_up_corner: Vec, val num_floors: Int, screen: Screen with MultiController) {
  val building_height = num_floors*floor_height + 20

  private val _floors = Array.fill(num_floors)(ArrayBuffer[Passenger]())
  def floors:Seq[Seq[Passenger]] = _floors
  def passengers:Seq[Passenger] = _floors.flatMap(f => f.toSeq)

  def issuePassenger(moment:Long) {
    val floor = if(math.random < 0.3) 0 else 1+(math.random*(num_floors-2)).toInt
    val target_floor = if (floor == 0) 1+(math.random*(num_floors-2)).toInt else {
      if(math.random < 0.3) 0
      else (1 to num_floors-1).filterNot(_ == floor)((math.random*(num_floors-2)).toInt)
    }
    _floors(floor) += new Passenger(floor, target_floor, moment)
  }

  private val _elevators = ArrayBuffer[Elevator]()
  def elevators:Seq[Elevator] = _elevators

  def addElevator(human_capacity:Int) {
    val e_leftup = if(_elevators.isEmpty) {
      left_up_corner + Vec(2*floor_width, -10)
    } else _elevators.last.left_up_corner + Vec(floor_width + 10, 0)
    val e = new Elevator(e_leftup, num_floors, human_capacity, screen)
    _elevators += e
  }

  def buildingWidth = 2*floor_width + floor_width*_elevators.length + 10*(_elevators.length)

  private val render_func = screen.render {
    currentColor = WHITE
    drawRect(left_up_corner, buildingWidth, building_height)

    val floor_lines = (0 to num_floors).map {
      x => List(Vec(left_up_corner.x,                 left_up_corner.y - (num_floors - x) * floor_height - 10),
                Vec(left_up_corner.x + buildingWidth, left_up_corner.y - (num_floors - x) * floor_height - 10))
    }.flatten
    drawGroupedLines(floor_lines)

    (0 until num_floors).foreach(num => {
      print(_floors(num).length, left_up_corner + Vec(floor_width, floor_height/2-10 - floor_height*(num_floors - num)), align = "center")
    })
  }

  private val action_func = screen.action(10) {
    _elevators.withFilter(e => !e.isMoving && e.availableSpace > 0 && _floors(e.currentFloor).length > 0).foreach(e => {
      val e_floor = e.currentFloor
      _floors(e_floor) --= e.enter(_floors(e_floor))
    })
  }

  private var removed = false
  def remove() {
    if(!removed) {
      removed = true
      screen.delOperations(render_func, action_func)
    }
  }

  screen.clear {
    remove()
    screen.deleteSelf()
  }
}

/**
 * Нумерация этажей: от 0 до num_floors-1
 *
 * @param left_up_corner - верхний левый угол шахты лифта
 * @param num_floors - количество этажей
 * @param capacity - количество людей, которое вмещает лифт
 * @param screen - экран, на котором действует объект лифта
 */
class Elevator(val left_up_corner: Vec, val num_floors: Int, val capacity:Int, screen: Screen with MultiController) {
  val elevator_height = num_floors * floor_height
  val door_width = floor_width - 14

  private var pos           = posForFloor(num_floors-1)
  def currentFloor = floorForCoord(pos)

  private val _passengers = ArrayBuffer[Passenger]()
  def passengers:Seq[Passenger] = _passengers
  def availableSpace = capacity - _passengers.length
  def isFree = _passengers.length == 0
  def isFull = _passengers.length == capacity

  def enter(passengers_from_floor:Seq[Passenger]):Seq[Passenger] = {
    if(_passengers.length < capacity) {
      val to_enter = passengers_from_floor.take(capacity - _passengers.length)
      _passengers ++= to_enter
      to_enter
    } else Seq()
  }

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

    print(_passengers.length+"/"+capacity, left_up_corner + Vec(5, 15))

    drawRectCentered(pos, floor_width - 7, floor_height - 7)
    drawDoor()

    if(moving_step.notZero) {
      drawRectCentered(target_floor_pos, floor_width+3, floor_height+3, RED)
    }

    _passengers.foreach(p => {
      val p_target_floor_pos = posForFloor(p.target_floor)
      drawRectCentered(p_target_floor_pos, floor_width+4, floor_height+4, YELLOW)
    })
  }

  private val left_mouse = screen.leftMouseOnRect(left_up_corner, floor_width, elevator_height, onBtnDown = mouse => {
    if(MainMenu.manualMode) moveToFloor(floorForCoord(mouse))
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

  def isMoving     = moving_step.notZero
  def isMovingDown = moving_step.y < 0
  def isMovingUp   = moving_step.y > 0
  def targetFloor  = floorForCoord(target_floor_pos)
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
                        val transported_passengers = _passengers.filter(_.target_floor == floor)
                        if(transported_passengers.length > 0) {
                          _passengers --= transported_passengers
                          callEvent("transported", transported_passengers)
                        }
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
    if(!removed) {
      removed = true
      screen.delOperations(render_func, left_mouse)
    }
  }

  screen.clear {
    remove()
    screen.deleteSelf()
  }
}

class LiftDriverAI(building:Building, screen:Screen) {
  private val action_func = screen.action(100) {
    // находим пассажиров, к которым ни один лифт еще не едет
    val waiting_passengers = building.passengers.filter(p => {
      building.elevators.forall(l => l.targetFloor != p.floor)
    })

    waiting_passengers.foreach(p => {
      // находим лифты, которые никуда не едут, и где нет пассажиров и сортируем их по близости к пассажиру
      val available_lifts = building.elevators
        .filter(lift => !lift.isMoving && lift.isFree)
        .sortBy(lift => {
          math.abs(p.target_floor - lift.currentFloor)
      })

      if(available_lifts.nonEmpty) {  // если такие есть
        // выбираем ближайший
        val nearest_lift = available_lifts.head

        // посылаем его к пассажиру
        nearest_lift.moveToFloor(p.floor)
      }
    })

    val (moving_lifts, non_moving_lifts) = building.elevators.partition(_.isMoving)
    non_moving_lifts.foreach(lift => { // если лифт не движется
      if(lift.passengers.nonEmpty) { // если в лифте есть пассажиры
        // выбираем пассажира, нужный этаж которого ближе всего
        val nearest_passenger = lift.passengers.sortBy(p => {
          math.abs(p.target_floor - lift.currentFloor)
        }).head

        // едем туда
        lift.moveToFloor(nearest_passenger.target_floor)
      } else {  // если в лифте нет пассажиров. По идее, эта секция излишняя, потому что такие лифты мы уже послали выше
        // находим пассажиров, к которым ни один лифт еще не едет
        val waiting_passengers = building.passengers.filter(p => {
          building.elevators.forall(l => l.targetFloor != p.floor)
        })

        if(waiting_passengers.nonEmpty) {  // если такие есть
          // выбираем ближайшего
          val nearest_passenger = waiting_passengers.sortBy(p => {
            math.abs(p.floor - lift.currentFloor)
          }).head

          // едем к нему
          lift.moveToFloor(nearest_passenger.floor)
        }
      }
    })

    moving_lifts.foreach(l => {   // если лифт движется
      if(l.availableSpace > 0) {  // если в лифте еще есть место
        // если едем вниз, то ищем пассажиров, которые ожидают лифт на этажах под нами, но выше того этажа,
        // на который мы в данный момент едем и к кому никто еще не едет
        // если же едем вверх, то ищем пассажиров, которые ожидают лифт на этажах под нами, но выше того этажа,
        // на который мы в данный момент едем и к кому никто еще не едет
        val waiting_passengers = building.passengers.filter(p => {
          if(l.isMovingDown) p.floor < l.currentFloor && p.floor > l.targetFloor && building.elevators.forall(l => l.targetFloor != p.floor)
          else if(l.isMovingUp) p.floor > l.currentFloor && p.floor < l.targetFloor && building.elevators.forall(l => l.targetFloor != p.floor)
          else false
        })
        if(waiting_passengers.nonEmpty) { // если такие есть
          // выбираем ближайшего
          val nearest_passenger = waiting_passengers.sortBy(p => {
            math.abs(p.target_floor - l.currentFloor)
          }).head

          // едем к нему
          l.moveToFloor(nearest_passenger.floor)
        }
      }
    })
  }

  private var removed = false
  def remove() {
    if(!removed) {
      removed = true
      screen.delOperation(action_func)
    }
  }

  screen.clear {
    remove()
    screen.deleteSelf()
  }
}
