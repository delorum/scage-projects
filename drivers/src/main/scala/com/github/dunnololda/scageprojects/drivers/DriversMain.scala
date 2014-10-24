package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.drivers.RoadMap._

import scala.collection.mutable.ArrayBuffer

// 5px = 1m

object DriversMain extends ScageScreenApp("Drivers", 800, 600) {
  val map_name = "map.txt"
  loadMap(map_name)

  private val road_points = RoadMap.road_network.keys.toList
  private val car = new Car("car", road_network.keys.head, 0, this)

  private val path = ArrayBuffer[Vec]()

  private var need_wheel_rotation = 0f
  private val def_vector = Vec(0,1)

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

  action {
    if(path.nonEmpty) {
      val dist = path.head.dist(car.carCenter) / 5f // дистанция в метрах
      if(dist < 3) path.remove(0)
    }

    if(path.isEmpty) {
      val p1 = road_points.sortBy(v => v.dist(car.carCenter)).head
      val p2 = road_points((math.random*road_points.length).toInt)
      val new_path = dijkstra1(p1, RoadMap.road_network.map(kv => (kv._1, kv._2.toList)).toMap).getOrElse(p2, Nil)
      /*val reduced_path = if(path.length > 2) {
        path.tail.sliding(2).foldLeft((List(path.head), (path.tail.head - path.head).n)) {
          case ((res, cur_dir), List(from, to)) =>
            val dir = (to - from).n
            if(dir == cur_dir) (res, cur_dir) else (res ::: List(from), dir)
        }
      } else (path, Vec.zero)
      reduced_path._1.foreach(ai_car.addWayPoint)*/
      path ++= new_path
    } else if(path.length < 20) {
      val p1 = path.last
      val p2 = road_points((math.random*road_points.length).toInt)
      val new_path = dijkstra1(p1, RoadMap.road_network.map(kv => (kv._1, kv._2.toList)).toMap).getOrElse(p2, Nil)
      path ++= new_path
    }

    need_wheel_rotation = if(path.nonEmpty) {
      val need_rotation = def_vector.signedDeg(path.head - car.carCenter)
      val tmp1 = need_rotation - car.rotation
      val tmp = if(math.abs(tmp1) < 180) tmp1 else (360 - math.abs(tmp1))*math.signum(tmp1)*(-1)
      val dist = path.head.dist(car.carCenter) / 5f      // дистанция в метрах
      if(tmp < 0) math.max(tmp, -45) else math.min(tmp, 45)
    } else 0
  }

  private val car_points = ArrayBuffer[Vec]()
  private val future_car_points = ArrayBuffer[Vec]()

  render {
    map_elements.zipWithIndex.foreach {
      case (road, idx) =>
        road.drawSelf(WHITE)
    }
    /*road_network.foreach {
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
    }*/
    path.zipWithIndex.foreach(pos => {
      if(pos._2 == 0) drawFilledCircle(pos._1, 5, RED)
      else drawFilledCircle(pos._1, 3, WHITE)
    })
    path.sliding(2).foreach {
      case Seq(from, to) =>
        drawLine(from, to, WHITE)
      case _ =>
    }

    val bs = car.bodyState(1)
    openglLocalTransform {
      openglMove(bs.coord)
      openglRotateDeg(bs.ang.toFloat)
      drawRectCentered(Vec.zero, 9, 23.3f, GRAY)
    }
    car_points += car.carCenter
    if(car_points.length > 5000) car_points.remove(0, 1000)
    if(car_points.length > 1) car_points.sliding(2).foreach {case Seq(f, t) => drawLine(f, t, WHITE)}

    future_car_points += bs.coord
    if(future_car_points.length > 5000) future_car_points.remove(0, 1000)
    if(future_car_points.length > 1) future_car_points.sliding(2).foreach {case Seq(f, t) => drawLine(f, t, GRAY)}
  }

  interface {
    //print(f"rotation radius: ${18.03f/math.sin(car.frontWheelsRotation.toRad)/5f}%.1f m", 20, 80, WHITE)

    print(f"wheels rotation: ${car.frontWheelsRotation}%.0f deg", 20, 100, WHITE)
    print(f"need wheel rotation is $need_wheel_rotation%.1f deg", 20, 80, WHITE)
    print(f"rotation: ${car.rotation}%.0f deg", 20, 60, WHITE)
    if(path.nonEmpty) {
      val need_rotation = def_vector.signedDeg(path.head - car.carCenter)
      print(f"need rotation is $need_rotation%.1f deg", 20, 40, WHITE)
    }
    print(f"speed: ${car.speed/5}%.0f m/s; ${car.speed/5*3.6}%.0f km/h", 20, 20, WHITE)
  }
}

