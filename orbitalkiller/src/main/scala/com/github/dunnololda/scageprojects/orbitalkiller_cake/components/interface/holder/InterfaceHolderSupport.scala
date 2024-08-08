package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.holder

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.CelestialsAware

trait InterfaceHolderSupport extends InterfaceHolderAware with CelestialsAware {
  protected val interfaceHolder = new InterfaceHolder(celestialsHelper)
}