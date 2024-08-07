package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware

trait ShipsHolderSupport extends ShipsHolderAware with SystemEvolutionAware with CelestialsAware {
  protected val shipsHolder = new ShipsHolder(systemEvolution, sun, earth, moon)
}
