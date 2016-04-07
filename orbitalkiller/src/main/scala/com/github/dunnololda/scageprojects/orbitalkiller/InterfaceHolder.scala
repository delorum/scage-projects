package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scageprojects.orbitalkiller.interface.elements._
import OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers._
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
  val namesSwitcher = new NamesOnOff
  val dockingSwitcher = new DockingOnOff
  val dockUndock = new DockUndock

  val timeInfo = new TimeInfo

  val viewModeInfo = new ViewModeInfo
  val flightModeInfo = new FlightModeInfo

  val sunRelativeInfo = new SunRelativeInfo
  val earthRelativeInfo = new EarthRelativeInfo
  val moonRelativeInfo = new MoonRelativeInfo
  
  val stationInfo = new OtherShipInfo(OrbitalKiller.station)
  val sat1Info = new OtherShipInfo(OrbitalKiller.sat1)
  def shipsMinimized = List(stationInfo, sat1Info).filter(_.isMinimized)

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
    List(sunRelativeInfo, earthRelativeInfo, moonRelativeInfo),
    List(stationInfo, sat1Info),
    List(linearVelocityInfo, angularVelocityInfo, pilotStateInfo),
    List(planetsInfluenceInfo, /*satelliteEscapeVelocityInfo, */orbitInfo),
    List(enginesInfo),
    List(shipParamsWhenEginesOff)
  )

  private val switchers = List[InterfaceSwitcher](
    msecOrKmH,
    orbParams,
    namesSwitcher,
    dockingSwitcher,
    dockUndock)
  private def activeSwitchers = switchers.filter(_.active)

  def hideAllByUser(): Unit = {
    interfaces.flatten.foreach(_.hideByUser())
  }

  def showAllByUser(): Unit = {
    interfaces.flatten.foreach(_.showByUser())
  }

  def clickInterfaceElem(mouse_coord:DVec):Boolean = {
    if(10 < mouse_coord.y && mouse_coord.y < 30) {  // мышкой кликнули на линии свернутых элементов
      var x_offset = 20                     // левый край очередной надписи по x
      _minimized_strings.map {
        case (i, color) =>
          val s = i.shortDescr              // надпись
          val s_len = messageBounds(s).ix     // длина надписи
          val x = x_offset+s_len/2            // центр надписи по x
          print(s, x, 20, ship.colorIfAliveOrRed(color), align = "center")
          x_offset = x_offset + s_len + between_minimized_elems
          ((i, color), x)
      }.find {
        case ((i, color), minimized_elem_center_x) =>
          minimized_elem_center_x - 20 < mouse_coord.x && mouse_coord.x < minimized_elem_center_x + 20
      }.exists(x => {
        val i = x._1._1
        if(i.isInstanceOf[OtherShipInfoMinimized]) {
          shipsMinimized.headOption.foreach(_.showByUser())
        } else {
          i.showByUser()
        }
        true
      })
    } else if(30 < mouse_coord.y && mouse_coord.y < 50) {  // мышкой кликнули на линии переключателей
      activeSwitchers.zipWithIndex.find {
        case (sw, idx) =>
          val minimized_elem_center_x = 30 + idx * between_switchers
          minimized_elem_center_x - 20 < mouse_coord.x && mouse_coord.x < minimized_elem_center_x + 20
      }.exists(x => {
        x._1.switch(); true
      })
    } else {                                       // кликаем на развернутых элементах интерфейса
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
    if(ship.flightMode != FreeFlightMode || !ship.engines.exists(_.active)) {
      shipParamsWhenEginesOff.hideByConstraint()
    } else {
      shipParamsWhenEginesOff.showByConstraint()
    }
    if(ship.flightMode == NearestPlanetVelocity || ship.flightMode == NearestShipAutoDocking) {
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
    var minimized_ship_exists = false
    var first_minimized_ship_position = 0
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
              if(!minimized_ship_exists && i.isInstanceOf[OtherShipInfo]) {
                minimized_ship_exists = true
                first_minimized_ship_position = _minimized_strings.length
              }
            }
        }
      if(non_minimized_exists) {
        _strings += (("", YELLOW))
        offset += 20
      }
    }
    if(minimized_ship_exists) {
      val minimized_ships = _minimized_strings.filter(_._1.isInstanceOf[OtherShipInfo])
      _minimized_strings.insert(first_minimized_ship_position, (new OtherShipInfoMinimized(minimized_ships.length), MAGENTA))
      _minimized_strings --= minimized_ships
    }
  }

  private val between_minimized_elems = 40
  private val between_switchers = 100

  def draw(): Unit = {
    _strings.zipWithIndex.foreach {
      case ((str, color), idx) => print(str, 20, (_strings.length+2 - idx)*20, ship.colorIfAliveOrRed(color))
    }
    activeSwitchers.zipWithIndex.foreach {
      case (switcher, idx) =>
        print(switcher.selectedStrVariant, 30+idx*between_switchers, 40, ship.colorIfAliveOrRed(YELLOW), align = "center")
    }
    //print(minimizedStrings.map(_._1).mkString(" "), 20, 20, DARK_GRAY)
    var x_offset = 20                     // левый край очередной надписи по x
    _minimized_strings.foreach {
      case (i, color) =>
        val s = i.shortDescr              // надпись
        val s_len = messageBounds(s).ix   // длина надписи
        val x = x_offset+s_len/2          // центр надписи по x
        print(s, x, 20, ship.colorIfAliveOrRed(color), align = "center")
        x_offset = x_offset + s_len + between_minimized_elems
    }
  }
}
