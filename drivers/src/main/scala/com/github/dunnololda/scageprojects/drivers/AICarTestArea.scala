package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.drivers.RoadMap._

object AICarTestArea extends ScageScreenApp("AI Car Test Area", 800, 600) {
  private val map_name = "map.txt"
  loadMap(map_name)

  private val road_points = RoadMap.road_network.keys.toList
  private val ai_car = new AICar(road_points((math.random*road_points.length).toInt), 45, this)
  //private val ai_car = new AICar(Vec(400, 300), 0, this)

  private var _center = windowCenter

  key(KEY_W, 100, onKeyDown = _center += Vec(0, 5))
  key(KEY_A, 100, onKeyDown = _center -= Vec(5, 0))
  key(KEY_S, 100, onKeyDown = _center -= Vec(0, 5))
  key(KEY_D, 100, onKeyDown = _center += Vec(5, 0))

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouse(onBtnDown = m => {
    ai_car.addWayPoint(scaledCoord(m))
  })

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 1) globalScale -= 1
    else if(globalScale > 0.1f) globalScale -= 0.1f
  })

  globalScale = 3
  center = _center

  action {
    if(ai_car.path.isEmpty) {
      val p1 = road_points.sortBy(v => v.dist(ai_car.carCenter)).head
      val p2 = road_points((math.random*road_points.length).toInt)
      val path = dijkstra1(p1, RoadMap.road_network.map(kv => (kv._1, kv._2.toList)).toMap).getOrElse(p2, Nil)
      /*val reduced_path = if(path.length > 2) {
        path.tail.sliding(2).foldLeft((List(path.head), (path.tail.head - path.head).n)) {
          case ((res, cur_dir), List(from, to)) =>
            val dir = (to - from).n
            if(dir == cur_dir) (res, cur_dir) else (res ::: List(from), dir)
        }
      } else (path, Vec.zero)
      reduced_path._1.foreach(ai_car.addWayPoint)*/
      path.foreach(ai_car.addWayPoint)
    } else if(ai_car.path.length < 20) {
      val p1 = ai_car.path.last
      val p2 = road_points((math.random*road_points.length).toInt)
      val path = dijkstra1(p1, RoadMap.road_network.map(kv => (kv._1, kv._2.toList)).toMap).getOrElse(p2, Nil)
      path.foreach(ai_car.addWayPoint)
    }
  }

  render {
    map_elements.zipWithIndex.foreach {
      case (road, idx) =>
        road.drawSelf(WHITE)
    }
    ai_car.path.foreach(pos => drawFilledCircle(pos, 3, WHITE))
    ai_car.path.sliding(2).foreach {
      case Seq(from, to) =>
        drawLine(from, to, WHITE)
      case _ =>
    }
  }

  interface {
    //print(f"rotation radius: ${18.03f/math.sin(car.frontWheelsRotation/180f*math.Pi)/5f}%.1f m", 20, 80, WHITE)
    ai_car.path.headOption.foreach(pos => print(f"distance: ${ai_car.carCenter.dist(pos)/5f}%.0f m", 500, 80, WHITE))
    print(f"wheels rotation: ${ai_car.frontWheelsRotation}%.0f deg", 500, 60, WHITE)
    print(f"rotation: ${ai_car.rotation}%.0f deg", 500, 40, WHITE)
    print(f"speed: ${ai_car.speed/5}%.0f m/s; ${ai_car.speed/5*3.6}%.0f km/h", 500, 20, WHITE)
  }
}
