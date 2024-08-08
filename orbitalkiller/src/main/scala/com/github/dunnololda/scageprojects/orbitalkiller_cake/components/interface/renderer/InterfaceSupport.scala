package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.renderer

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.holder.InterfaceHolderAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.main_screen.MainScreenAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.saveload.SaveLoadAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.ShipsAware

trait InterfaceSupport
  extends InterfaceAware
  with MainScreenAware
  with ShipsAware
  with InterfaceHolderAware
  with SaveLoadAware {

  override protected val interfaceRenderer: InterfaceRenderer =
    new InterfaceRenderer(mainScreen, interfaceHolder, saveLoadComponent)
}
