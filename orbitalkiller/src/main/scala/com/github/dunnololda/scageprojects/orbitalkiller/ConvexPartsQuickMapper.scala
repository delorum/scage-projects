package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.cli.AppProperties
import com.github.dunnololda.scage.ScageLibD._

import scala.collection.mutable.ArrayBuffer

object ConvexPartsQuickMapper extends ScageScreenAppD("ConvexPartsQuickMapper", property("screen.width", 640), property("screen.height", 480)) {
  val ship_class_name = AppProperties.stringProperty("ship")

  val ship = {
    val constructor = Class.forName(s"com.github.dunnololda.scageprojects.orbitalkiller.ships.$ship_class_name").getConstructors()(0)
    val args = Array(new java.lang.Integer(1), DVec.zero, DVec.zero, new java.lang.Double(0), new java.lang.Boolean(true))
    constructor.newInstance(args: _*).asInstanceOf[PolygonShip]
  }

  /*if(ship.convex_parts.nonEmpty) {
    ship.convex_parts.zipWithIndex.foreach {
      case (convex_part, idx) =>
        if(!isCCW(convex_part.points)) {
          println(s"$idx convex part is not ccw!")
        }
    }
  }

  if(ship.wreck_parts.nonEmpty) {
    ship.wreck_parts.zipWithIndex.foreach {
      case (wreck_part, idx) =>
        if(!isCCW(wreck_part.points)) {
          println(s"$idx wreck part is not ccw!")
        }
    }
  }*/

  private val k = ship.engine_size.toFloat * 2

  val ship_points = ship.points.map(p => p / k + windowCenter)
  val ship_draw_points = ship.draw_points.map(p => p / k + windowCenter)

  val whole_coords = (ship_points.head.x.toFloat / k) % 1 == 0

  private val cell_size: Float = 1.0f
  private val cell_size2 = cell_size * cell_size
  private val cell_size_double = cell_size * 2
  private val cell_size_half: Float = 0.5f * cell_size
  private val cell_size_quater: Float = 0.25f * cell_size
  private val cell_size_eights: Float = 0.125f * cell_size
  private lazy val points = collection.mutable.ArrayBuffer[DVec]()
  private var selected_point = 0

  private val mapped = ArrayBuffer[List[DVec]](/*ship.convex_parts.map(_.points.map(p => p / k + DVec(windowWidth / 2, windowHeight / 2))):_**/)

  // ВАЖНО: следует перечислять точки ПРОТИВ ЧАСОВОЙ СТРЕЛКИ! Если по часовой перечислять, collision detection будет плохо работать

  private def nearestDot(x: Double, a: Double, h: Double): Double = {
    val x1 = a + ((x - a) / h).toInt * h
    val x2 = x1 + h
    if (x - x1 < x2 - x) x1 else x2
  }

  def isCCW(points: Seq[DVec]): Boolean = {
    points.length > 2 && {
      val c = points.sum / points.length
      (points :+ points.head).map(_ - c).sliding(2).map {
        case Seq(p1, p2) =>
          // http://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order
          (p2.x - p1.x) * (p2.y + p1.y)
      }.sum < 0
    }
  }

  key(KEY_SPACE, onKeyDown = {
    if (points.length > 2) {
      val ps = points.reverse
      if (isCCW(ps)) {
        println(s"PolygonShape(List(${ps.map(p => s"DVec(${(p.x - windowWidth / 2).toFloat * k}, ${(p.y - windowHeight / 2).toFloat * k})").mkString(", ")}), Nil),")
        mapped += points.toList
      } else {
        println("// NOT COUNTER-CLOCKWISE!")
      }
    }
    points.clear()
    selected_point = 0
  })

  key(KEY_C, onKeyDown = {
    println("=====================================")
    mapped.clear()
  })

  key(KEY_Q, onKeyDown = if (keyPressed(KEY_RCONTROL) || keyPressed(KEY_LCONTROL)) stopApp())

  leftMouse(onBtnDown = m => {
    val ssm = absCoord(m)
    if (points.forall(p => p.dist2(ssm) > cell_size2)) {
      val sm = Vec(nearestDot(ssm.x, -windowWidth / 2, cell_size) - (if (!whole_coords) 0.5 else 0.0),
        nearestDot(ssm.y, -windowHeight / 2, cell_size) - (if (!whole_coords) 0.5 else 0.0))
      points.insert(selected_point, sm)
    }
  }, onBtnUp = m => {
    val ssm = absCoord(m)
    val sm = Vec(nearestDot(ssm.x, -windowWidth / 2, cell_size) - (if (!whole_coords) 0.5 else 0.0),
      nearestDot(ssm.y, -windowHeight / 2, cell_size) - (if (!whole_coords) 0.5 else 0.0))
    points(selected_point) = sm
  })

  rightMouse(onBtnDown = m => {
    if (points.length > 0) {
      points.remove(selected_point)
    }
    if (points.length > 0) selected_point = points.length - 1
    else selected_point = 0
  })

  mouseWheelDown(onWheelDown = m => {
    selected_point -= 1
    if (selected_point < 0) selected_point = points.length - 1
  })

  mouseWheelUp(onWheelUp = m => {
    selected_point += 1
    if (selected_point >= points.length) selected_point = 0
  })

  leftMouseDrag(onDrag = m => {
    val ssm = absCoord(m)
    points.zipWithIndex.find(p => p._1.dist2(ssm) < cell_size2).foreach(p => {
      selected_point = p._2
      points(selected_point) = ssm
    })
  })

  //center = DVec.zero
  globalScale = 20f / cell_size

  render {
    if (whole_coords) {
      (0f to windowWidth by cell_size).foreach(x => drawLine(Vec(x, 0), Vec(x, windowHeight), DARK_GRAY))
      (0f to windowHeight by cell_size).foreach(y => drawLine(Vec(0, y), Vec(windowWidth, y), DARK_GRAY))
    } else {
      (0.5f to windowWidth by cell_size).foreach(x => drawLine(Vec(x, 0.5), Vec(x, windowHeight), DARK_GRAY))
      (0.5f to windowHeight by cell_size).foreach(y => drawLine(Vec(0.5, y), Vec(windowWidth, y), DARK_GRAY))
    }

    ship_points.zipWithIndex.foreach {
      case (p, idx) =>
        drawFilledCircle(p / k, 3 / globalScale, GRAY)
    }
    drawSlidingLines(ship_draw_points, GRAY)

    mapped.foreach(pp => {
      drawFilledPolygon(pp, GRAY)
      //drawSlidingLines(pp.:+(pp.head), WHITE)
    })

    if (points.length > 0) {
      points.zipWithIndex.foreach(p => drawFilledCircle(p._1, 0.3f * cell_size, if (p._2 == selected_point) RED else WHITE))
      drawSlidingLines(points.:+(points.head), WHITE)
      val c = points.sum / points.length
      drawFilledCircle(c, 3 / globalScale, WHITE)
    }
  }
}
