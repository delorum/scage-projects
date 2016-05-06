package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.OrbitalKiller._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.elements._
import com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
  val gSwitcher = new GSwitcher
  val degOrKm = new DegOrKm
  val timeStepSwitcher = new TimeStepSwitcher

  val timeInfo = new TimeInfo

  val viewModeInfo = new ViewModeInfo
  val flightModeInfo = new FlightModeInfo

  val sunRelativeInfo = new SunRelativeInfo
  val earthRelativeInfo = new EarthRelativeInfo
  val moonRelativeInfo = new MoonRelativeInfo
  
  val stationInfo = new OtherShipInfo(OrbitalKiller.station)
  val sat1Info = new OtherShipInfo(OrbitalKiller.sat1)
  val ship_interfaces = List(stationInfo, sat1Info)
  def shipsMinimized = ship_interfaces.filter(_.isMinimized)

  val rocketsInfo = new RocketsInfo

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
    ship_interfaces,
    List(rocketsInfo),
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
    dockUndock,
    gSwitcher,
    degOrKm,
    timeStepSwitcher)
  private def activeSwitchers = switchers.filter(_.active)

  def hideAllByUser(): Unit = {
    interfaces.flatten.foreach(_.hideByUser())
  }

  def showAllByUser(): Unit = {
    interfaces.flatten.foreach(_.showByUser())
  }

  /**
   * метод обрабатывает клики мыши на элементах интерфейса. Правая или левая кнопки - имеет смысл только для переключателей, у которых
   * опций выбора больше двух. Для них левая кнопка перелючает вперед, правая - назад (чтобы можно было не проматывать долго вперед).
   * Во всех остальных случаях кликать надо левой кнопкой.
   * @param mouse_coord - координата клика мыши
   * @param mouse_btn - 0 - левая кнопка, 1 - правая кнопка мыши
   * @return - true/false. true - попали в какой-то элемент интерфейса, и событие было обработано.
   */
  def clickInterfaceElem(mouse_coord:DVec, mouse_btn:Int):Boolean = {
    if(10 < mouse_coord.y && mouse_coord.y < 30) {  // мышкой кликнули на линии свернутых элементов
      mouse_btn == 0 && {
        _minimized_strings.find {
          case (i, color, minimized_elem_center_x) =>
            minimized_elem_center_x - i.shortDescrLen / 2 < mouse_coord.x && mouse_coord.x < minimized_elem_center_x + i.shortDescrLen / 2
        }.exists(x => {
          val i = x._1
          if (i.isInstanceOf[OtherShipInfoMinimized]) {
            shipsMinimized.headOption.foreach(_.showByUser())
          } else {
            i.showByUser()
          }
          true
        })
      }
    } else if(30 < mouse_coord.y && mouse_coord.y < 50) {  // мышкой кликнули на линии переключателей
      var switchers_x_offset = 20
      activeSwitchers.zipWithIndex.find {
        case (sw, idx) =>
          val switcher_center_x = switchers_x_offset + sw.selectedStrVariantLen/2
          switchers_x_offset += sw.selectedStrVariantLen + between_switchers
          switcher_center_x - sw.selectedStrVariantLen/2 < mouse_coord.x && mouse_coord.x < switcher_center_x + sw.selectedStrVariantLen/2
      }.exists(x => {
        if(mouse_btn == 1) {
          x._1.switchBack()
        } else if(mouse_btn == 0) {
          x._1.switchForward()
        }
        true
      })
    } else if(20 < mouse_coord.x && mouse_coord.x < 300) {  // кликаем на развернутых элементах интерфейса
      mouse_btn == 0 && {
        val x = (_strings.length + 2) * 20 - mouse_coord.y
        _interface_elems_positions.find {
          case (i, pos) =>
            pos - 20 < x && x < pos + 20 * (i.data.length - 1)
        }.exists(x => {
          x._1.hideByUser(); true
        })
      }
    } else false
  }

  def constraints(): Unit = {
    /*if(ship.otherShipsNear.isEmpty) {
      nearestShipInfo.hideByConstraint()
    } else nearestShipInfo.showByConstraint()*/
    if(player_ship.flightMode != FreeFlightMode || !player_ship.engines.exists(_.active)) {
      shipParamsWhenEginesOff.hideByConstraint()
    } else {
      shipParamsWhenEginesOff.showByConstraint()
    }
    if(player_ship.flightMode == NearestPlanetVelocity || player_ship.flightMode == NearestShipAutoDocking) {
      enginesInfo.hideByConstraint()
    } else {
      enginesInfo.showByConstraint()
    }
    if(player_ship.isLanded) {
      angularVelocityInfo.hideByConstraint()
    } else {
      angularVelocityInfo.showByConstraint()
    }
  }

  private val _strings = ArrayBuffer[(String, ScageColor)]()
  //def strings:Seq[(String, ScageColor)] = _strings

  private val _minimized_strings = ArrayBuffer[(InterfaceElement, ScageColor, Int)]()
  //def minimizedStrings:Seq[(InterfaceElement, ScageColor)] = _minimized_strings

  private val _interface_elems_positions = ArrayBuffer[(InterfaceElement, Int)]()

  def update(): Unit = {
    constraints()
    interfaces.flatten.foreach(_.updateIfNotMinimized())
    _strings.clear()
    _minimized_strings.clear()
    _interface_elems_positions.clear()
    var offset = 0
    var minimized_x_offset = 20                         // левый край очередной свернутой надписи по x
    var minimized_ship_exists = false
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
              if(!i.isInstanceOf[OtherShipInfo]) {
                val x_position = minimized_x_offset+i.shortDescrLen/2    // центр надписи по x
                if(i.isMinimizedByConstraint) {
                  _minimized_strings += ((i, DARK_GRAY, x_position))
                } else {
                  _minimized_strings += ((i, i.color, x_position))
                }
                minimized_x_offset += i.shortDescrLen + between_minimized_elems
              } else {
                if(!minimized_ship_exists) {
                  val j = new OtherShipInfoMinimized(shipsMinimized.length)
                  val x = minimized_x_offset+j.shortDescrLen/2    // центр надписи по x
                  _minimized_strings += ((j, j.color, x))
                  minimized_ship_exists = true
                  minimized_x_offset += j.shortDescrLen + between_minimized_elems
                }
              }
            }
        }
      if(non_minimized_exists) {
        _strings += (("", YELLOW))
        offset += 20
      }
    }
  }

  private val between_minimized_elems = 20
  private val between_switchers = 20

  def draw(): Unit = {
    _strings.zipWithIndex.foreach {
      case ((str, color), idx) => print(str, 20, (_strings.length+2 - idx)*20, player_ship.colorIfPlayerAliveOrRed(color))
    }
    var switchers_x_offset = 20
    activeSwitchers.zipWithIndex.foreach {
      case (switcher, idx) =>
        val x_position = switchers_x_offset + switcher.selectedStrVariantLen/2
        print(switcher.selectedStrVariant, x_position, 40, player_ship.colorIfPlayerAliveOrRed(YELLOW), align = "center")
        switchers_x_offset += switcher.selectedStrVariantLen + between_switchers
    }
    //print(minimizedStrings.map(_._1).mkString(" "), 20, 20, DARK_GRAY)
    _minimized_strings.foreach {
      case (interface_elem, color, x_position) =>
        print(interface_elem.shortDescr, x_position, 20, player_ship.colorIfPlayerAliveOrRed(color), align = "center")
    }
  }
}
