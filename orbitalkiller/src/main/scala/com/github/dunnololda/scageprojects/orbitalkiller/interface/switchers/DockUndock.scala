package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4

class DockUndock(dockingSwitcher: DockingOnOff, playerShip: Ship4) extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("Dock", "Undock")

  def setDocked(): Unit = {
    selected_variant = 1
  }

  def needUndock = {
    selected_variant == 0
  }

  def needDock = {
    selected_variant == 1
  }

  override def active: Boolean = {
    (dockingSwitcher.dockingEnabled && playerShip.canDockWithNearestShip) || playerShip.isDocked
  }
}
