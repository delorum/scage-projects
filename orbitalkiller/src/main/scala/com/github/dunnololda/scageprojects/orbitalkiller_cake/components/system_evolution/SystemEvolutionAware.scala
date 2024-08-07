package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution

import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.SystemEvolution

trait SystemEvolutionAware {
  protected def systemEvolution: SystemEvolution

  trait SystemEvolutionAwareImpl extends SystemEvolutionAware {
    override protected def systemEvolution: SystemEvolution = SystemEvolutionAware.this.systemEvolution
  }
}
