package com.github.dunnololda.scageprojects.orbitalkiller.interface.switchers

import com.github.dunnololda.scageprojects.orbitalkiller.InterfaceSwitcher

/**
 * Переключатель - с каким максимальным ускорением можно двигаться. Этой настройке будут следовать все автоматические системы корабля
 */
class GSwitcher extends InterfaceSwitcher {
  override def strVariants: Array[String] = Array("Goff", "1g", "4g", "8g", "10g")
  selected_variant = 2

  def maxGSet:Boolean = selected_variant != 0

  def maxG = selected_variant match {
    case 0 => -1
    case 1 => 1
    case 2 => 4
    case 3 => 8
    case 4 => 10
    case _ => -1
  }
}
