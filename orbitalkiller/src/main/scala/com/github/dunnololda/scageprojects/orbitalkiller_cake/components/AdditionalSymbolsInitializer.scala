package com.github.dunnololda.scageprojects.orbitalkiller_cake.components

import com.github.dunnololda.scage.ScageLibD.addGlyphs
import com.github.dunnololda.scageprojects.orbitalkiller_cake.AdditionalSymbols
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main.preinit
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.main_screen.MainScreenAware

trait AdditionalSymbolsInitializer extends MainScreenAware {
  preinit {
    addGlyphs(AdditionalSymbols.all)
  }
}
