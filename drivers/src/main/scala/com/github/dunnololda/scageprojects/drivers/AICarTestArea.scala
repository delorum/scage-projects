package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.drivers.RoadMap._

import scala.collection.mutable.ArrayBuffer

object AICarTestArea extends ScageScreenApp("AI Car Test Area", 800, 600) {
  private val map_name = "map.txt"
  loadMap(map_name)

  val seconds = 2


  private val road_points = RoadMap.road_network.keys.toList

  val cars = ArrayBuffer[AICar]()
  def addAICar(index:String) {
    val pos = road_points.filter(p => cars.forall(c => c.carCenter.dist(p) > 25))
    if(pos.nonEmpty) {
      val x = pos((math.random*pos.length).toInt)
      val rot = road_network.get(x).map(l => {
        val y = l((math.random*l.length).toInt)
        (y - x).n
      }).getOrElse(def_vector)
      cars += new AICar(index, x+rot*(-20), def_vector.signedDeg(rot), this)
    }
  }
  (1 to 20).foreach(i => addAICar(s"ai_car$i"))

  private var ai_car  = cars.head

  private def generatePathForCar(car:AICar): Unit = {
    if(car.path.isEmpty) {
      val p1 = road_points.sortBy(v => v.dist(car.carCenter)).head
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
      car.addPath(path)
    } else if(car.path.length < 20) {
      val p1 = car.path.last
      val p2 = road_points((math.random*road_points.length).toInt)
      val path = dijkstra1(p1, RoadMap.road_network.map(kv => (kv._1, kv._2.toList)).toMap).getOrElse(p2, Nil)
      car.addPath(path)
    }
  }

  private var _center = windowCenter

  key(KEY_W, 100, onKeyDown = _center += Vec(0, 5))
  key(KEY_A, 100, onKeyDown = _center -= Vec(5, 0))
  key(KEY_S, 100, onKeyDown = _center -= Vec(0, 5))
  key(KEY_D, 100, onKeyDown = _center += Vec(5, 0))

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouse(onBtnDown = m => {
    val pos = scaledCoord(m)
    ai_car = cars.sortBy(_.carCenter.dist(pos)).head
    _center = ai_car.carCenter
  })

  mouseWheelUp(onWheelUp = m => {
    if(globalScale < 1) globalScale += 0.1f
    else if(globalScale < 4) globalScale += 1
  })
  mouseWheelDown(onWheelDown = m => {
    if(globalScale > 1) globalScale -= 1
    else if(globalScale > 0.1f) globalScale -= 0.1f
  })

  center = _center

  //center = ai_car.carCenter/*+Vec(0,100)*/
  globalScale = 3
  /*rotationPoint = ai_car.carCenter
  rotationAngleDeg = -ai_car.rotation*/

  action(1000) {
    cars.foreach(generatePathForCar)
  }

  render {
    map_elements.zipWithIndex.foreach {
      case (road, idx) =>
        road.drawSelf(WHITE)
    }
    ai_car.path.zipWithIndex.foreach(pos => {
      if(pos._2 == 0) drawFilledCircle(pos._1, 5, RED)
      else drawFilledCircle(pos._1, 3, WHITE)
    })
    ai_car.path.sliding(2).foreach {
      case Seq(from, to) =>
        drawLine(from, to, WHITE)
      case _ =>
    }

    cars.combinations(2).foreach {
      case Seq(car1, car2) =>
        maybeCollision(car1.bodyState(0), car2.bodyState(0)).foreach {
          case Contact(b1, b2, contact_point, normal) =>
            println("collision!")
            drawFilledCircle(contact_point, 3, GREEN)
        }
    }

    /*cars.foreach {
      case car =>
        val bs = car.bodyState(seconds)
        openglLocalTransform {
          openglMove(bs.coord)
          openglRotateDeg(bs.ang.toFloat)
          drawRectCentered(Vec.zero, 9, 23.3f, GRAY)
        }
    }*/

    val bs1 = ai_car.bodyState(seconds)
    openglLocalTransform {
      openglMove(bs1.coord)
      openglRotateDeg(bs1.ang.toFloat)
      drawRectCentered(Vec.zero, 9, 23.3f, RED)
    }
    val bs2 = ai_car.bodyState(seconds, ai_car.needSpeed, ai_car.needWheelsRotation)
    openglLocalTransform {
      openglMove(bs2.coord)
      openglRotateDeg(bs2.ang.toFloat)
      drawRectCentered(Vec.zero, 9, 23.3f, YELLOW)
    }
    val bs3 = ai_car.bodyState(seconds, -5f/3.6f*5f, 0)
    openglLocalTransform {
      openglMove(bs3.coord)
      openglRotateDeg(bs3.ang.toFloat)
      drawRectCentered(Vec.zero, 9, 23.3f, GREEN)
    }
  }

  interface {
    print(f"distance to next point: ${ai_car.distToNextPoint}%.0f m",      500, 60, WHITE)
    print(f"distance to next turn: ${ai_car.distToNextTurn}%.0f m",        500, 40, WHITE)
    print(f"speed: ${ai_car.speed/5}%.0f m/s; ${ai_car.speed/5*3.6}%.0f km/h",   500, 20, WHITE)

    print(f"rotation is ${ai_car.rotation}%.1f deg",                        20, 80, WHITE)
    print(f"need rotation is ${ai_car.needRotation}%.1f deg",                 20, 60, WHITE)
    print(f"wheels rotation: ${ai_car.frontWheelsRotation}%.0f deg",      20, 40, WHITE)
    print(f"need wheels rotation is ${ai_car.needWheelsRotation}%.1f deg",   20, 20, WHITE)
  }
}
