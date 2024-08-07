package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships

import com.github.dunnololda.scageprojects.orbitalkiller.ships.{Cargo1, Satellite1, Satellite2, Ship4, SpaceStation2}

trait ShipsAware {
  protected def playerShip: Ship4

  protected def station: SpaceStation2

  protected def sat1: Satellite1

  protected def sat2: Satellite2

  protected def cargo1: Cargo1

  trait ShipsAwareImpl extends ShipsAware {
    override protected def playerShip: Ship4 = ShipsAware.this.playerShip

    override protected def station: SpaceStation2 = ShipsAware.this.station

    override protected def sat1: Satellite1 = ShipsAware.this.sat1

    override protected def sat2: Satellite2 = ShipsAware.this.sat2

    override protected def cargo1: Cargo1 = ShipsAware.this.cargo1
  }
}
