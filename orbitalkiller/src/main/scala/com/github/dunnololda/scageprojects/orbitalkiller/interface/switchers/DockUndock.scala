package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.{OrbitalKiller, InterfaceHolder, InterfaceSwitcher}

class DockUndock extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("Dock, Undock")

  override def active:Boolean = {
    InterfaceHolder.dockingSwitcher.dockingEnabled && OrbitalKiller.ship.canDockWithNearestShip
  }
}
