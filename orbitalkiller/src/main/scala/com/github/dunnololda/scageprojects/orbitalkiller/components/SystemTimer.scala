package com.github.dunnololda.scageprojects.orbitalkiller.components

import com.github.dunnololda.scageprojects.orbitalkiller.physics.SystemEvolution
import BasicComponents.base_dt

/**
  * TODO
  *
  * @author aborunov
  */
class SystemTimer(system_evolution: SystemEvolution) extends TimeAware {
  def tacts: Long = system_evolution.tacts

  def timeMsec: Long = (tacts * base_dt * 1000).toLong
}
