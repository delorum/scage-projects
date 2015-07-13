package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements._
import OrbitalKiller._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import com.github.dunnololda.scage.ScageLibD._

object InterfaceHolder {
  private val additional_messages = mutable.HashMap[String, String]()
  def addMessage(keyword:String, message:String): Unit = {
    additional_messages += (keyword -> message)
  }
  def removeMessage(keyword:String): Unit = {
    additional_messages -= keyword
  }

  val timeInfo = new TimeInfo

  val viewModeInfo = new ViewModeInfo
  val flightModeInfo = new FlightModeInfo

  val earthRelativeInfo = new EarthRelativeInfo
  val moonRelativeInfo = new MoonRelativeInfo
  val nearestShipInfo = new NearestShipInfo

  val linearVelocityInfo = new LinearVelocityInfo
  val angularVelocityInfo = new AngularVelocityInfo
  val pilotStateInfo = new PilotStateInfo

  val planetsInfluenceInfo = new PlanetsInfluenceInfo
  val satelliteEscapeVelocityInfo = new SatelliteEscapeVelocityInfo
  val orbitInfo = new OrbitInfo

  val enginesInfo = new EnginesInfo

  val shipParamsWhenEginesOff = new ShipParamsWhenEnginesOff

  val interfaces = List(
    List(timeInfo),
    List(viewModeInfo, flightModeInfo),
    List(earthRelativeInfo, moonRelativeInfo, nearestShipInfo),
    List(linearVelocityInfo, angularVelocityInfo, pilotStateInfo),
    List(planetsInfluenceInfo, satelliteEscapeVelocityInfo, orbitInfo),
    List(enginesInfo),
    List(shipParamsWhenEginesOff)
  )

  def hideAllByUser(): Unit = {
    interfaces.flatten.foreach(_.hideByUser())
  }

  def showAllByUser(): Unit = {
    interfaces.flatten.foreach(_.showByUser())
  }

  def determineInterfaceElem(mouse_coord:DVec):Option[InterfaceElement] = {
    if(10 < mouse_coord.y && mouse_coord.y < 30) {
      _minimized_strings.zipWithIndex.find {
        case ((i, color), idx) =>
          val pos = 20 + idx*40
          pos < mouse_coord.x && mouse_coord.x < pos+40
      }.map(_._1._1)
    } else {
      val x = (_strings.length + 2) * 20 - mouse_coord.y
      _interface_elems_positions.find {
        case (i, pos) =>
          pos - 20 < x && x < pos + 20 * (i.data.length - 1)
      }.map(_._1)
    }
  }

  def constraints(): Unit = {
    if(ship.otherShipsNear.isEmpty) {
      nearestShipInfo.hideByConstraint()
    } else nearestShipInfo.showByConstraint()
    if(ship.flightMode == 8) {
      enginesInfo.hideByConstraint()
      shipParamsWhenEginesOff.hideByConstraint()
    } else {
      enginesInfo.showByConstraint()
      if(!ship.engines.exists(_.active) || ship.flightMode == 0) {
        shipParamsWhenEginesOff.hideByConstraint()
      } else {
        shipParamsWhenEginesOff.showByConstraint()
      }
    }
  }

  private val _strings = ArrayBuffer[String]()
  def strings:Seq[String] = _strings

  private val _minimized_strings = ArrayBuffer[(InterfaceElement, ScageColor)]()
  def minimizedStrings:Seq[(InterfaceElement, ScageColor)] = _minimized_strings

  private val _interface_elems_positions = ArrayBuffer[(InterfaceElement, Int)]()

  def update(): Unit = {
    constraints()
    interfaces.flatten.foreach(_.updateIfNotMinimized())
    _strings.clear()
    _minimized_strings.clear()
    _interface_elems_positions.clear()
    var offset = 0
    interfaces.foreach {
      case l =>
        var non_minimized_exists = false
        l.foreach {
          case i =>
            if(!i.isMinimized) {
              non_minimized_exists = true
              _strings ++= i.data
             _interface_elems_positions += ((i, offset))
              offset += i.data.length*20
            } else {
              if(i.isMinimizedByConstraint) {
                _minimized_strings += ((i, DARK_GRAY))
              } else {
                _minimized_strings += ((i, YELLOW))
              }
            }
        }
      if(non_minimized_exists) {
        _strings += ""
        offset += 20
      }
    }
  }
}
