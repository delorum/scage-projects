package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements._
import OrbitalKiller._
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

  val timeInfo = new TimeInfo

  val viewModeInfo = new ViewModeInfo
  val flightModeInfo = new FlightModeInfo

  val earthRelativeInfo = new EarthRelativeInfo
  val moonRelativeInfo = new MoonRelativeInfo
  val nearestShipInfo = new NearestShipInfo

  val linearVelocityInfo = new LinearVelocityInfo
  val angularVelocityInfo = new AngularVelocityInfo

  val planetsInfluenceInfo = new PlanetsInfluenceInfo
  val satelliteEscapeVelocityInfo = new SatelliteEscapeVelocityInfo
  val orbitInfo = new OrbitInfo

  val enginesInfo = new EnginesInfo

  val shipParamsWhenEginesOff = new ShipParamsWhenEnginesOff

  val interfaces = List(
    List(timeInfo),
    List(viewModeInfo, flightModeInfo),
    List(earthRelativeInfo, moonRelativeInfo, nearestShipInfo),
    List(linearVelocityInfo, angularVelocityInfo),
    List(planetsInfluenceInfo, satelliteEscapeVelocityInfo, orbitInfo),
    List(enginesInfo),
    List(shipParamsWhenEginesOff)
  )

  def constraints(): Unit = {
    if(ship.otherShipsNear.isEmpty) {
      nearestShipInfo.hide()
    }
    if(ship.flightMode == 8) {
      enginesInfo.hide()
      shipParamsWhenEginesOff.hide()
    }
    if(!ship.engines.exists(_.active) || ship.flightMode == 0) {
      shipParamsWhenEginesOff.hide()
    }
  }

  private var _update_needed = true
  def markUpdateNeeded(): Unit = {
    _update_needed = true
  }

  private val _strings = ArrayBuffer[String]()
  def strings:Seq[String] = _strings

  def updateIfNeeded(): Unit = {
    if (_update_needed) {
      constraints()
      interfaces.flatten.foreach(_.update())
      _strings.clear()
      interfaces.foreach {
        case l => l.foreach {
          case i =>
            _strings ++= i.data
        }
          _strings += ""
      }
    }
    _update_needed = false
  }
}
