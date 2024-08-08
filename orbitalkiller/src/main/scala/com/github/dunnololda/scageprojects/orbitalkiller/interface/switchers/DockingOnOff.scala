package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

class DockingOnOff extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("Doff", "DonAuto", "DonMan")

  def dockingEnabled: Boolean = selected_variant != 0

  def dockingAuto: Boolean = selected_variant == 1

  def dockingManual: Boolean = selected_variant == 2

  def setDockingManual(): Unit = {
    selected_variant = 2
  }

  def setDockingAuto(): Unit = {
    selected_variant = 1
  }
}
