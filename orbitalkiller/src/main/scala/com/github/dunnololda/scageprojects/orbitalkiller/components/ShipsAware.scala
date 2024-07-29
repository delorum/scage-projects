package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.ships.{Ship4, ShipsHolder}

/**
  * Created by andrey on 1/7/18.
  */
trait ShipsAware {
  def player_ship: Ship4

  def shipsHolder: ShipsHolder
}
