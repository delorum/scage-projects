package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements._
import OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.switchers.{OrbitParamsCalculation, MSecOrKmH}
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

  val msecOrKmH = new MSecOrKmH
  val orbParams = new OrbitParamsCalculation

  val timeInfo = new TimeInfo

  val viewModeInfo = new ViewModeInfo
  val flightModeInfo = new FlightModeInfo

  val sunRelativeInfo = new SunRelativeInfo
  val earthRelativeInfo = new EarthRelativeInfo
  val moonRelativeInfo = new MoonRelativeInfo
  val nearestShipInfo = new ShipInfo(OrbitalKiller.station)

  val linearVelocityInfo = new LinearVelocityInfo
  val angularVelocityInfo = new AngularVelocityInfo
  val pilotStateInfo = new ShipAndCrewStateInfo

  val planetsInfluenceInfo = new PlanetsInfluenceInfo
  //val satelliteEscapeVelocityInfo = new SatelliteEscapeVelocityInfo
  val orbitInfo = new OrbitInfo

  val enginesInfo = new EnginesInfo

  val shipParamsWhenEginesOff = new ShipParamsWhenEnginesOff

  private val interfaces = List(
    List(timeInfo),
    List(viewModeInfo, flightModeInfo),
    List(sunRelativeInfo, earthRelativeInfo, moonRelativeInfo, nearestShipInfo),
    List(linearVelocityInfo, angularVelocityInfo, pilotStateInfo),
    List(planetsInfluenceInfo, /*satelliteEscapeVelocityInfo, */orbitInfo),
    List(enginesInfo),
    List(shipParamsWhenEginesOff)
  )

  private val switchers = List[InterfaceSwitcher](msecOrKmH, orbParams)
  //def switchers:Seq[InterfaceSwitcher] = _switchers

  def hideAllByUser(): Unit = {
    interfaces.flatten.foreach(_.hideByUser())
  }

  def showAllByUser(): Unit = {
    interfaces.flatten.foreach(_.showByUser())
  }

  def clickInterfaceElem(mouse_coord:DVec):Boolean = {
    if(10 < mouse_coord.y && mouse_coord.y < 30) {  // мышкой кликнули на линии свернутых элементов
      _minimized_strings.zipWithIndex.find {
        case ((i, color), idx) =>
          val minimized_elem_center_x = 20 + idx * between_minimized_elems
          minimized_elem_center_x - 20 < mouse_coord.x && mouse_coord.x < minimized_elem_center_x + 20
      }.exists(x => {
        x._1._1.showByUser(); true
      })
    } else if(30 < mouse_coord.y && mouse_coord.y < 50) {  // мышкой кликнули на линии переключателей
      switchers.zipWithIndex.find {
        case (sw, idx) =>
          val minimized_elem_center_x = 30 + idx * between_switchers
          minimized_elem_center_x - 20 < mouse_coord.x && mouse_coord.x < minimized_elem_center_x + 20
      }.exists(x => {
        x._1.switch(); true
      })
    } else {
      if(20 < mouse_coord.x && mouse_coord.x < 300) {
        val x = (_strings.length + 2) * 20 - mouse_coord.y
        _interface_elems_positions.find {
          case (i, pos) =>
            pos - 20 < x && x < pos + 20 * (i.data.length - 1)
        }.exists(x => {x._1.hideByUser(); true})
      } else false
    }
  }

  def constraints(): Unit = {
    /*if(ship.otherShipsNear.isEmpty) {
      nearestShipInfo.hideByConstraint()
    } else nearestShipInfo.showByConstraint()*/
    if(ship.flightMode != Free || !ship.engines.exists(_.active)) {
      shipParamsWhenEginesOff.hideByConstraint()
    } else {
      shipParamsWhenEginesOff.showByConstraint()
    }
    if(ship.flightMode == NearestPlanetVelocity) {
      enginesInfo.hideByConstraint()
    } else {
      enginesInfo.showByConstraint()
    }
  }

  private val _strings = ArrayBuffer[(String, ScageColor)]()
  //def strings:Seq[(String, ScageColor)] = _strings

  private val _minimized_strings = ArrayBuffer[(InterfaceElement, ScageColor)]()
  //def minimizedStrings:Seq[(InterfaceElement, ScageColor)] = _minimized_strings

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
              _strings ++= i.data.map(x => (x, i.color))
             _interface_elems_positions += ((i, offset))
              offset += i.data.length*20
            } else {
              if(i.isMinimizedByConstraint) {
                _minimized_strings += ((i, DARK_GRAY))
              } else {
                _minimized_strings += ((i, i.color))
              }
            }
        }
      if(non_minimized_exists) {
        _strings += (("", YELLOW))
        offset += 20
      }
    }
  }

  private val between_minimized_elems = 40
  private val between_switchers = 100

  def draw(): Unit = {
    _strings.zipWithIndex.foreach {
      case ((str, color), idx) => print(str, 20, (_strings.length+2 - idx)*20, ship.colorIfAliveOrRed(color))
    }
    switchers.zipWithIndex.foreach {
      case (switcher, idx) =>
        print(switcher.selectedStrVariant, 30+idx*between_switchers, 40, YELLOW, align = "center")
    }
    //print(minimizedStrings.map(_._1).mkString(" "), 20, 20, DARK_GRAY)
    _minimized_strings.zipWithIndex.foreach {
      case ((i, color), idx) =>
        print(i.shortDescr, 20+idx*between_minimized_elems, 20, ship.colorIfAliveOrRed(color), align = "center")
    }
  }
}
