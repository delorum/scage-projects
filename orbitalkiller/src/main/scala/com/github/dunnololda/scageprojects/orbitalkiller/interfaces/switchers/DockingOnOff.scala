package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.interfaces.InterfaceSwitcher

class DockingOnOff extends InterfaceSwitcher {
  override val strVariants: Array[String] = Array("Doff", "DonAuto", "DonMan")

  def dockingEnabled = selected_variant != 0

  def dockingAuto = selected_variant == 1

  def dockingManual = selected_variant == 2

  def setDockingManual() {
    selected_variant = 2
  }

  def setDockingAuto() {
    selected_variant = 1
  }
}
