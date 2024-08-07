package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials

import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.{Planet, PlanetWithAir, Star}

trait CelestialsAware {
  protected def sun: Star

  protected def earth: PlanetWithAir

  protected def moon: Planet

  protected def celestialsHelper: CelestialsHelper

  trait CelestialsAwareImpl extends CelestialsAware {
    override protected def sun: Star = CelestialsAware.this.sun

    override protected def earth: PlanetWithAir = CelestialsAware.this.earth

    override protected def moon: Planet = CelestialsAware.this.moon

    override protected def celestialsHelper: CelestialsHelper = CelestialsAware.this.celestialsHelper
  }
}
