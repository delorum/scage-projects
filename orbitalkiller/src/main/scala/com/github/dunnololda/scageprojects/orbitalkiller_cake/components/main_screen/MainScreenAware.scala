package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.main_screen

import com.github.dunnololda.scage.handlers.RendererD

trait MainScreenAware {

  protected def mainScreen: RendererD

  trait MainScreenAwareImpl extends MainScreenAware {

    override protected def mainScreen: RendererD = MainScreenAware.this.mainScreen
  }
}
