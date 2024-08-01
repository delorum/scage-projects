package com.github.dunnololda.scageprojects.orbitalkiller_cake.render

sealed trait ViewMode {
  def rusStr: String
}

object ViewMode {
  case object FreeViewMode extends ViewMode {
    override def rusStr: String = "свободный"
  }

  case object FixedOnShip extends ViewMode {
    override def rusStr: String = "фиксация на корабле"
  }

  case object FixedOnShipAbsolute extends ViewMode {
    override def rusStr: String = "фиксация на корабле, абсолютная ориентация"
  }

  case object Landing extends ViewMode {
    override def rusStr: String = "посадка на планету"
  }

  case object FixedOnOrbit extends ViewMode {
    override def rusStr: String = "фиксация на орбите корабля"
  }
}