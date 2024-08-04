package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder

trait ProtectedShipsHolderAware {
  protected def shipsHolder: ShipsHolder

  trait ProtectedShipsHolderAwareImpl extends ProtectedShipsHolderAware {
    protected def shipsHolder: ShipsHolder = ProtectedShipsHolderAware.this.shipsHolder
  }
}
