package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.drivers.RoadMap._

// 5px = 1m

object DriversMain extends ScageScreenApp("Drivers", 800, 600) {
  val map_name = "map.txt"
  loadMap(map_name)

  private val car = new Car(road_network.keys.head, 0, this)

  key(KEY_UP,     50, onKeyDown = car.speed += 0.5f)
  key(KEY_DOWN,   50, onKeyDown = car.speed -= 0.5f)
  key(KEY_SPACE,  25, onKeyDown = {
    if(car.speed != 0) {
      if(car.speed < 0) {
        car.speed += 1
        if(car.speed > 0) car.speed = 0
      }
      if(car.speed > 0) {
        car.speed -= 1
        if(car.speed < 0) car.speed = 0
      }
    }

  })
  key(KEY_RIGHT,   5, onKeyDown = {car.is_wheel_rotating = true; if(car.frontWheelsRotation > -45) car.frontWheelsRotation -= 1}, onKeyUp = car.is_wheel_rotating = false)
  key(KEY_LEFT,    5, onKeyDown = {car.is_wheel_rotating = true; if(car.frontWheelsRotation <  45) car.frontWheelsRotation += 1}, onKeyUp = car.is_wheel_rotating = false)

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  center = car.carCenter/*+Vec(0,100)*/
  globalScale = 3
  rotationPoint = car.carCenter
  rotationAngleDeg = -car.rotation

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 1) globalScale -= 1
    else if(globalScale > 0.1f) globalScale -= 0.1f
  })

  render {
    map_elements.zipWithIndex.foreach {
      case (road, idx) =>
        road.drawSelf(WHITE)
    }
    road_network.foreach {
      case (elem, connected_to) =>
        val color = GREEN
        drawRectCentered(elem, 5, 5, color)
        connected_to.foreach {
          case to =>
            drawLine(elem, to, color)
            val v1 = (elem - to).n.rotateDeg(15)*3
            val v2 = (elem - to).n.rotateDeg(-15)*3
            drawLine(to, to+v1, color)
            drawLine(to, to+v2, color)
        }
    }
  }

  interface {
    //print(f"rotation radius: ${18.03f/math.sin(car.frontWheelsRotation/180f*math.Pi)/5f}%.1f m", 20, 80, WHITE)
    //print(f"wheels rotation: ${car.frontWheelsRotation}%.0f deg", 20, 60, WHITE)
    print(f"rotation: ${car.rotation}%.0f deg", 20, 40, WHITE)
    print(f"speed: ${car.speed/5}%.0f m/s; ${car.speed/5*3.6}%.0f km/h", 20, 20, WHITE)
  }
}

