package com.github.dunnololda.scageprojects.orbitalkiller

import com.github.dunnololda.scageprojects.orbitalkiller.interfaceElements._

object InterfaceHolder {
  val gameSavedInfo = new GameSavedInfo
  val gameLoadedInfo = new GameLoadedInfo
  val failedToLoadInfo = new FailedToLoadInfo
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
}
