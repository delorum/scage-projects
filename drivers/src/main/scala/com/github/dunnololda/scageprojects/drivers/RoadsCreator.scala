package com.github.dunnololda.scageprojects.drivers

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scageprojects.drivers.RoadMap._
import scala.collection.mutable.ArrayBuffer

object RoadsCreator extends ScageScreenApp("Roads Creator", 1920, 1200) {
  private val map_name = "protvino.txt"

  private val ROADS_MODE = 1
  private val CROSSROADS_MODE = 2
  private val NETWORK_MODE = 3
  private val PATH_FINDING = 4
  private val NAVIGATION = 0
  
  private var mode = ROADS_MODE
  
  private var _center = windowCenter
  private var _road_start:Option[Vec] = None

  private var selected_map_element = -1

  private var selected_network_element:Option[Vec] = None

  private var find_path_from:Option[Vec] = None
  private var found_path:List[Vec] = Nil

  loadMap(map_name)

  key(KEY_1, onKeyDown = mode = ROADS_MODE)
  key(KEY_2, onKeyDown = mode = CROSSROADS_MODE)
  key(KEY_3, onKeyDown = mode = NETWORK_MODE)
  key(KEY_4, onKeyDown = mode = PATH_FINDING)
  key(KEY_0, onKeyDown = mode = NAVIGATION)

  key(KEY_W, 100, onKeyDown = _center += Vec(0, 5))
  key(KEY_A, 100, onKeyDown = _center -= Vec(5, 0))
  key(KEY_S, 100, onKeyDown = _center -= Vec(0, 5))
  key(KEY_D, 100, onKeyDown = _center += Vec(5, 0))

  key(KEY_LEFT, onKeyDown = {
    if(map_elements.nonEmpty) {
      selected_map_element += 1
      if(selected_map_element >= map_elements.length) selected_map_element = -1
    } else selected_map_element = -1
  })
  key(KEY_RIGHT, onKeyDown = {
    if(map_elements.nonEmpty) {
      selected_map_element -= 1
      if(selected_map_element <= -2) selected_map_element = map_elements.length-1
    } else selected_map_element = -1
  })

  key(KEY_ESCAPE, onKeyDown = {
    mode match {
      case NETWORK_MODE => selected_network_element = None
      case _ =>
    }
  })

  key(KEY_F5, onKeyDown = {
    RoadMap.saveMap(map_name)
  })

  key(KEY_Q, onKeyDown = {if(keyPressed(KEY_LCONTROL)) stopApp()})

  leftMouse(onBtnDown = m => {
    mode match {
      case ROADS_MODE =>
        _road_start match {
          case Some(from) =>
            val to = nearestPoint(scaledCoord(m))
            addRoad(from, to)
            _road_start = None
          case None =>
            _road_start = Some(nearestPoint(scaledCoord(m)))
        }
      case CROSSROADS_MODE =>
        val pos = nearestPoint(scaledCoord(m))
        /*val four_ends = Set(
          pos + Vec( 30,   0),
          pos + Vec(-30,   0),
          pos + Vec(  0,  30),
          pos + Vec(  0, -30)
        )
        val roads = road_elements.filter {
          case TwoLinesEachSide(from, to) => four_ends.contains(from) || four_ends.contains(to)
          case _ => false
        }.toList*/
        map_elements += CrossRoad(pos/*, roads*/)
        map_elements.find {
          case FourLaneRoad(from, to) =>
            (pos - from).n == (to - from).n
          case _ => false
        }.foreach {
          case er @ FourLaneRoad(from, to) =>
            removeRoad(er)
            val dir = (to - from).n
            addRoad(from, pos+dir*(-30))
            addRoad(pos+dir*30, to)
          case _ =>
        }
      case NETWORK_MODE =>
        val pos = nearestPoint(scaledCoord(m))
        if(!road_network.contains(pos)) {
          road_network(pos) = ArrayBuffer[Vec]()
        }
        selected_network_element match {
          case Some(elem) if elem != pos =>
            road_network.getOrElseUpdate(elem, ArrayBuffer[Vec]()) += pos
          case _ =>
        }
        selected_network_element = Some(pos)
      case PATH_FINDING =>
        val pos = scaledCoord(m)
        if(find_path_from.isEmpty) {
          find_path_from = road_network.keys.toList.sortBy(_.dist2(pos)).headOption
          found_path = Nil
        } else {
          val find_path_to = road_network.keys.toList.sortBy(_.dist2(pos)).headOption
          if(find_path_to.nonEmpty) {
            found_path = dijkstra1(find_path_from.get, road_network.map(kv => (kv._1, kv._2.toList)).toMap).getOrElse(find_path_to.get, Nil)
          }
          find_path_from = None
        }
      case NAVIGATION =>
        _center = scaledCoord(m)
      case _ =>
    }
  })
  rightMouse(onBtnDown = m => {
    _road_start match {
      case Some(from) =>
        _road_start = None
      case None =>
        mode match {
          case ROADS_MODE | CROSSROADS_MODE =>
            if(selected_map_element != -1 && map_elements.nonEmpty && selected_map_element >= 0 && selected_map_element < map_elements.length) {
              map_elements(selected_map_element) match {
                case fr:FourLaneRoad => removeRoad(fr)
                case x => map_elements -= x
              }
              if(selected_map_element >= map_elements.length) selected_map_element = -1
            }
          case NETWORK_MODE =>
            selected_network_element match {
              case Some(elem) =>
                road_network -= elem
                road_network.foreach(kv => if(kv._2.contains(elem)) kv._2 -= elem)
                selected_network_element = None
              case None =>
            }
          case PATH_FINDING =>
            find_path_from = None
          case _ =>
        }
    }
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

  render {
    (-5000 to 5000 by 15).foreach(x => drawLine(Vec(x, -5000), Vec(x, 5000), DARK_GRAY))
    (-5000 to 5000 by 15).foreach(y => drawLine(Vec(-5000, y), Vec(5000, y), DARK_GRAY))
    map_elements.zipWithIndex.foreach {
      case (road, idx) =>
        if(idx == selected_map_element) {
          road.drawSelf(RED)
        } else {
          road.drawSelf(WHITE)
        }
    }
    road_network.foreach {
      case (elem, connected_to) =>
        val color = if(selected_network_element.exists(se => elem == se)) RED else GREEN
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
    find_path_from.foreach(p => drawCircle(p, 4, RED))
    found_path.sliding(2).foreach {
      case List(from ,to) =>
        drawCircle(from, 4, RED)
        drawCircle(to, 4, RED)
        drawLine(from ,to, RED)
      case List(from) =>
        drawCircle(from, 4, RED)
    }
    mode match {
      case ROADS_MODE =>
        if(_road_start.nonEmpty) {
          val from = _road_start.get
          val to = nearestPoint(scaledCoord(mouseCoord))
          drawLine(from, to, WHITE)
          val v1 = (from - to).n.rotateDeg(15)*3
          val v2 = (from - to).n.rotateDeg(-15)*3
          drawLine(to, to+v1, WHITE)
          drawLine(to, to+v2, WHITE)
          val n = (to - from).n
          val n_turn_1 = n.rotateDeg(90)
          val n_turn_2 = n.rotateDeg(-90)
          val meters_legend_n_turn = if(n_turn_1.y < 0) n_turn_1 else n_turn_2
          val meters_legend_rotation = if(meters_legend_n_turn == n_turn_2) {
            (to - from).deg(Vec(1,0))*math.signum(to.y-from.y)
          } else {
            (from - to).deg(Vec(1,0))*math.signum(from.y-to.y)
          }
          val road_length_meters = (to - from).norma/5f
          openglLocalTransform {
            openglMove(from+meters_legend_n_turn*10 + (to - from)/2f)
            openglRotate(meters_legend_rotation)
            print(f"$road_length_meters%.1f m", Vec.zero, max_font_size/RoadsCreator.globalScale, WHITE, "center")
          }
        } else {
          drawCircle(nearestPoint(scaledCoord(mouseCoord)), 5, WHITE)
        }
      case CROSSROADS_MODE =>
        drawRectCentered(nearestPoint(scaledCoord(mouseCoord)), 60, 60, WHITE)
      case NETWORK_MODE =>
        if(selected_network_element.nonEmpty) {
          val from = selected_network_element.get
          val to = nearestPoint(scaledCoord(mouseCoord))
          drawLine(from, to, GREEN)
          val v1 = (from - to).n.rotateDeg(15)*3
          val v2 = (from - to).n.rotateDeg(-15)*3
          drawLine(to, to+v1, GREEN)
          drawLine(to, to+v2, GREEN)
          val n = (to - from).n
          val n_turn_1 = n.rotateDeg(90)
          val n_turn_2 = n.rotateDeg(-90)

          val meters_legend_n_turn = if(n_turn_1.y < 0) n_turn_1 else n_turn_2
          val meters_legend_rotation = if(meters_legend_n_turn == n_turn_2) {
            (to - from).deg(Vec(1,0))*math.signum(to.y-from.y)
          } else {
            (from - to).deg(Vec(1,0))*math.signum(from.y-to.y)
          }
          val road_length_meters = (to - from).norma/5f
          openglLocalTransform {
            openglMove(from+meters_legend_n_turn*10 + (to - from)/2f)
            openglRotate(meters_legend_rotation)
            print(f"$road_length_meters%.1f m", Vec.zero, max_font_size/RoadsCreator.globalScale, GREEN, "center")
          }
        } else {
          drawRectCentered(nearestPoint(scaledCoord(mouseCoord)), 5, 5, GREEN)
        }
      case _ =>
    }
  }
  
  interface {
    mode match {
      case ROADS_MODE =>
        val p = nearestPoint(scaledCoord(mouseCoord))
        print(s"Mode: Roads ${_road_start.map(v => s"${v.x}:${v.y}").getOrElse("-")} -> ${p.x}:${p.y}", 20, 20, WHITE)
      case CROSSROADS_MODE =>
        print("Mode: Crossroads", 20, 20, WHITE)
      case NETWORK_MODE =>
        print("Mode: Network", 20, 20, WHITE)
      case PATH_FINDING =>
        print("Mode: Path Finding", 20, 20, WHITE)
      case NAVIGATION =>
        print("Mode: Navigation", 20, 20, WHITE)
      case _ =>
    }
  }

  private def nearestPoint(v:Vec):Vec = {
    Vec(v.ix/15*15-5, v.iy/15*15-5)
  }

  private def calculateRoadsToReplace(from:Vec, to:Vec, replaced_roads:Set[FourLaneRoad] = Set()):(Vec, Vec, Set[FourLaneRoad]) = {
    map_elements.find {
      case fr @ FourLaneRoad(exists_from, exists_to) if !replaced_roads.contains(fr) =>
        from == exists_to || from == exists_from ||
          to == exists_to   || to == exists_from
      case _ => false
    } match {
      case Some(er @ FourLaneRoad(exists_from, exists_to)) =>
        if(exists_to == from) {
          calculateRoadsToReplace(exists_from, to, replaced_roads + er)
        } else if(exists_to == to) {
          calculateRoadsToReplace(from, exists_from, replaced_roads + er)
        } else if(exists_from == to) {
          calculateRoadsToReplace(from, exists_to, replaced_roads + er)
        } else if(exists_from == from) {
          calculateRoadsToReplace(to, exists_to, replaced_roads + er)
        } else (from, to, replaced_roads)
      case _ => (from, to, replaced_roads)
    }
  }

  private def addRoad(from:Vec, to:Vec) {
    val (f, t, replaced_roads) = calculateRoadsToReplace(from, to)
    replaced_roads.foreach(removeRoad)
    map_elements += FourLaneRoad(f, t)
    val dir = (t - f).n
    val dir1 = dir.rotateDeg(-90).n*15
    val dir2 = dir.rotateDeg(90).n*15
    val road_len = (t - f).norma
    val network_points1 = (0f to road_len by 45f).map {
      case len => f + dir*len + dir1
    }.toList ::: {if(road_len % 45 == 0) Nil else List(t+dir1)}
    network_points1.zipWithIndex.init.foreach {
      case (np1, idx) => road_network(np1) = ArrayBuffer[Vec](network_points1(idx+1))
    }
    road_network(network_points1.last) = ArrayBuffer[Vec]()
    val network_points2 = (0f to road_len by 45f).map {
      case len => t - dir*len + dir2
    }.toList ::: {if(road_len % 45 == 0) Nil else List(f+dir2)}
    network_points2.zipWithIndex.init.foreach {
      case (np2, idx) => road_network(np2) = ArrayBuffer[Vec](network_points2(idx+1))
    }
    road_network(network_points2.last) = ArrayBuffer[Vec]()
  }

  private def removeRoad(road:FourLaneRoad) {
    val FourLaneRoad(from, to) = road
    val dir = (to - from).n
    val dir1 = dir.rotateDeg(-90).n*15
    val dir2 = dir.rotateDeg(90).n*15
    val road_len = (to - from).norma
    val network_points1 = (0f to road_len by 45f).map {
      case len => from + dir*len + dir1
    }.toList ::: {if(road_len % 45 == 0) Nil else List(to+dir1)}
    network_points1.foreach(np => road_network -= np)
    val network_points2 = (0f to road_len by 45f).map {
      case len => to - dir*len + dir2
    }.toList ::: {if(road_len % 45 == 0) Nil else List(from+dir2)}
    network_points2.foreach(np => road_network -= np)
    map_elements -= road
  }
}

sealed trait MapElement

trait DrawableElement {
  def drawSelf(color:ScageColor)
}
case class FourLaneRoad(from:Vec, to:Vec) extends MapElement with DrawableElement {
  val road_length_meters = (to - from).norma/5f
  val n = (to - from).n
  val n_turn_1 = n.rotateDeg(90)
  val n_turn_2 = n.rotateDeg(-90)

  val meters_legend_n_turn = if(n_turn_1.y < 0) n_turn_1 else n_turn_2
  val meters_legend_rotation = if(meters_legend_n_turn == n_turn_2) {
    (to - from).deg(Vec(1,0))*math.signum(to.y-from.y)
  } else {
    (from - to).deg(Vec(1,0))*math.signum(from.y-to.y)
  }

  override def drawSelf(color: ScageColor) {
    // двойная сплошная по центру
    drawLine(from+n_turn_1, to+n_turn_1, color)
    drawLine(from+n_turn_2, to+n_turn_2, color)

    // пунктирные линии
    drawDashedLine(from+n_turn_1*15, to+n_turn_1*15, 5, color)
    drawDashedLine(from+n_turn_2*15, to+n_turn_2*15, 5, color)

    // границы дороги
    drawLine(from+n_turn_1*30, to+n_turn_1*30, color)
    drawLine(from+n_turn_2*30, to+n_turn_2*30, color)

    drawMeters(color)
  }

  private def drawMeters(color:ScageColor) {
    // сноски
    drawLine(from+meters_legend_n_turn*30, from+meters_legend_n_turn*40, color)
    drawLine(to+meters_legend_n_turn*30, to+meters_legend_n_turn*40, color)

    // линия со стрелочками на обоих концах
    drawLine(from+meters_legend_n_turn*35, to+meters_legend_n_turn*35, color)
    drawArrow(from+meters_legend_n_turn*35, to+meters_legend_n_turn*35)
    drawArrow(to+meters_legend_n_turn*35, from+meters_legend_n_turn*35)

    // надпись
    openglLocalTransform {
      openglMove(from + meters_legend_n_turn * 40 + (to - from) / 2f)
      openglRotate(meters_legend_rotation)
      print(f"$road_length_meters%.1f m", Vec.zero, max_font_size / RoadsCreator.globalScale, WHITE, "center")
    }
  }

  private def drawArrow(x:Vec, y:Vec): Unit = {
    val v1 = (x - y).n.rotateDeg(15)*3
    val v2 = (x - y).n.rotateDeg(-15)*3
    drawLine(y, y+v1, WHITE)
    drawLine(y, y+v2, WHITE)
  }
}

case class CrossRoad(pos:Vec/*, roads:List[RoadElement]*/) extends MapElement with DrawableElement {
  val up_end    = pos+Vec(  0,  30)
  val down_end  = pos+Vec(  0, -30)
  val right_end = pos+Vec( 30,   0)
  val left_end  = pos+Vec(-30,   0)
  override def drawSelf(color: ScageColor) {
    /*roads.length match {
      case 2 =>
      case 3 =>
      case 4 =>
      case _ =>
    }*/
    drawRectCentered(pos, 60, 60, color)
  }
}

case class NetworkPoint(point:Vec, other_points:List[Vec]) extends MapElement {
  
}

