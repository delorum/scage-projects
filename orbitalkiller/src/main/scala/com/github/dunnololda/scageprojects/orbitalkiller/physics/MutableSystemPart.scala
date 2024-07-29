package com.github.dunnololda.scageprojects.orbitalkiller.physics

import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.MutableBodyState

case class MutableSystemPart(body: MutableBodyState,
                             force: (Long, EvolutionHelper) => DVec,
                             torque: (Long, EvolutionHelper) => Double)
