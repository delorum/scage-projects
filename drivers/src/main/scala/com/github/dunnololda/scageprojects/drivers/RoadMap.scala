package com.github.dunnololda.scageprojects.drivers

import java.io.FileOutputStream
import com.github.dunnololda.scage.support.Vec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

object RoadMap extends JavaTokenParsers {
  private def vecParser:Parser[Vec] = floatingPointNumber~":"~floatingPointNumber ^^ {case x~":"~y => Vec(x.toFloat, y.toFloat)}
  private def fourLaneRoadParser:Parser[FourLaneRoad] = "FourLaneRoad"~vecParser~vecParser ^^ {
    case "FourLaneRoad"~from~to => FourLaneRoad(from, to)
  }
  private def crossRoadParser:Parser[CrossRoad] = "CrossRoad"~vecParser ^^ {
    case "CrossRoad"~pos => CrossRoad(pos)
  }
  private def networkPointParser:Parser[NetworkPoint] = "NetworkPoint"~vecParser~"->"~rep(vecParser) ^^ {
    case "NetworkPoint"~point~"->"~points => NetworkPoint(point, points)
  }

  private def mapElementParser:Parser[MapElement] = fourLaneRoadParser | crossRoadParser | networkPointParser

  def parseMapElement(row:String):Option[MapElement] = {
    try {
      parseAll(mapElementParser, row) match {
        case Success(map_element, _) =>
          Some(map_element)
        case Failure(msg, _) =>
          None
        case Error(msg, _) =>
          None
      }
    } catch {
      case t:Throwable => None
    }
  }

  val map_elements = ArrayBuffer[MapElement with DrawableElement]()
  val road_network = mutable.HashMap[Vec, ArrayBuffer[Vec]]()

  def saveMap(map_name:String) {
    val fos = new FileOutputStream(map_name)
    map_elements.foreach {
      case FourLaneRoad(from, to) => fos.write(s"FourLaneRoad ${from.x}:${from.y} ${to.x}:${to.y}\n".getBytes)
      case CrossRoad(pos/*, roads*/) => fos.write(s"CrossRoad ${pos.x}:${pos.y}\n".getBytes)
    }
    road_network.foreach {
      case (network_point, connections) =>
        fos.write(s"NetworkPoint ${network_point.x}:${network_point.y} -> ${connections.map(p => s"${p.x}:${p.y}").mkString(" ")}\n".getBytes)
    }
    fos.close()
    println("map saved")
  }

  def loadMap(map_name:String) {
    try {
      val file_lines = io.Source.fromFile(map_name).getLines()
      map_elements.clear()
      road_network.clear()
      for {
        line <- file_lines
        map_element <- RoadMap.parseMapElement(line)
      } {
        map_element match {
          case flr@FourLaneRoad(from, to) => map_elements += flr
          case cr@CrossRoad(pos) => map_elements += cr
          case NetworkPoint(point, other_points) => road_network(point) = ArrayBuffer[Vec](other_points: _*)
          case _ =>
        }
      }
    } catch {
      case t:Throwable => println(s"faild to load map $map_name: $t")
    }
  }
}
