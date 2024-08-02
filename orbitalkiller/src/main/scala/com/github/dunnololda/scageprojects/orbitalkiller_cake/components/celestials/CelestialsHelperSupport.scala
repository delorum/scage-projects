package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware

trait CelestialsHelperSupport extends CelestialsHelperAware with SystemEvolutionAware {
  val celestialsHelper: CelestialsHelper = new CelestialsHelper(Celestials.allCelestials, systemEvolution)
}
