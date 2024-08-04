package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships

import com.github.dunnololda.scageprojects.orbitalkiller.ships.{Cargo1, Satellite1, Satellite2, Ship4, SpaceStation2}

trait ShipsAware {
  def player_ship: Ship4

  def station: SpaceStation2

  def sat1: Satellite1

  def sat2: Satellite2

  def cargo1: Cargo1
}
