package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder

trait ShipsHolderAware {
  protected def shipsHolder: ShipsHolder

  trait ShipsHolderAwareImpl extends ShipsHolderAware {
    override protected def shipsHolder: ShipsHolder = ShipsHolderAware.this.shipsHolder
  }
}
