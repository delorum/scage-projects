package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.saveload

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.main_screen.MainScreenAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder.ShipsHolderAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware

trait SaveLoadSupport
  extends SaveLoadAware
  with MainScreenAware
  with SystemEvolutionAware
  with CelestialsAware
  with ShipsHolderAware {

  override protected val saveLoadComponent = new SaveLoadComponent
    with MainScreenAwareImpl
    with SystemEvolutionAwareImpl
    with CelestialsAwareImpl
    with ShipsHolderAwareImpl
}
