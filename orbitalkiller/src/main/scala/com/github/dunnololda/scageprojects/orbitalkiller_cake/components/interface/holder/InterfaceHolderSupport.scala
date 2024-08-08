package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.holder

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.ShipsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder.ShipsHolderAware

trait InterfaceHolderSupport extends InterfaceHolderAware with CelestialsAware with ShipsHolderAware with ShipsAware {
  protected val interfaceHolder = new InterfaceHolder(celestialsHelper, shipsHolder, playerShip, sun, earth, moon)
}
