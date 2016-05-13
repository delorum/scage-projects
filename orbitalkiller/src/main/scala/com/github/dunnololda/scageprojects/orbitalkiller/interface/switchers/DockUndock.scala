package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceHolder, InterfaceSwitcher, OrbitalKiller}

class DockUndock extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("Dock", "Undock")

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
    InterfaceHolder.dockingSwitcher.dockingEnabled && OrbitalKiller.player_ship.canDockWithNearestShip || OrbitalKiller.player_ship.isDocked
  }
}
