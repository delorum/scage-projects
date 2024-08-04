package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder

trait ShipsHolderAware {
  def shipsHolder: ShipsHolder

  implicit val sh: ShipsHolderAware = this

  trait ShipsHolderAwareImpl extends ShipsHolderAware {
    override def shipsHolder: ShipsHolder = ShipsHolderAware.this.shipsHolder
  }

  trait ProtectedShipsHolderAwareImpl extends ProtectedShipsHolderAware {
    protected def shipsHolder: ShipsHolder = ShipsHolderAware.this.shipsHolder
  }
}
