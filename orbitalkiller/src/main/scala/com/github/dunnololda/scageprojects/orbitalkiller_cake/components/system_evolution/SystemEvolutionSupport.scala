package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution

import com.github.dunnololda.scageprojects.orbitalkiller_cake.Constants.base_dt
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.SystemEvolution

trait SystemEvolutionSupport extends SystemEvolutionAware {
  val systemEvolution: SystemEvolution = new SystemEvolution(base_dt)
}
