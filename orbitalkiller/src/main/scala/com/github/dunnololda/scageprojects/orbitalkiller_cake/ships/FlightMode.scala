package com.github.dunnololda.scageprojects.orbitalkiller_cake.ships

sealed trait FlightMode {
  def rusStr: String
}

object FlightMode {

  // 1
  case object FreeFlightMode extends FlightMode {
    override def rusStr: String = "свободный"
  }

  // 2
  case object Killrot extends FlightMode {
    override def rusStr: String = "запрет вращения"
  }

  // 3
  case object RelativeVelocityAligned extends FlightMode {
    override def rusStr: String = "ориентация по траектории"
  }

  // shift+3
  case object OppositeRelativeVelocityAligned extends FlightMode {
    override def rusStr: String = "ориентация против траектории"
  }

  // 4
  case object CirclularOrbit extends FlightMode {
    override def rusStr: String = "выход на круговую орбиту"
  }

  // 5
  case object NearestShipVelocity extends FlightMode {
    override def rusStr: String = "уравнять скорость с кораблем"
  }

  // 6
  case object NearestShipAligned extends FlightMode {
    override def rusStr: String = "ориентация на корабль"
  }

  // 7
  case object NearestShipAutoDocking extends FlightMode {
    override def rusStr: String = "стыковка с кораблем"
  }

  // 8
  case object NearestPlanetVelocity extends FlightMode {
    override def rusStr: String = "уравнять скорость с ближайшей планетой"
  }

  // 9
  // vacant

  // 0
  case object Maneuvering extends FlightMode {
    override def rusStr: String = "маневрирование"
  }
}
