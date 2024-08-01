package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.{InterfaceHolder, InterfaceSwitcher}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main

class DockUndock extends InterfaceSwitcher {
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
    (InterfaceHolder.dockingSwitcher.dockingEnabled && Main.player_ship.canDockWithNearestShip) || Main.player_ship.isDocked
  }
}
