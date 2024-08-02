package com.github.dunnololda.scageprojects.orbitalkiller_cake.components

import com.github.dunnololda.scage.handlers.RendererD
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.{CelestialsHelperSupport, CelestialsInitializer}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.main_screen.MainScreenAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionSupport

class OrbitalKillerComponents(val mainScreen: RendererD)
  extends MainScreenAware
  with SystemEvolutionSupport
  with AdditionalSymbolsInitializer
  with CelestialsInitializer
  with CelestialsHelperSupport
