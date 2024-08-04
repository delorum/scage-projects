package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution

import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.MutableBodyState

case class MutableSystemPart(body: MutableBodyState,
                             force: (Long, EvolutionHelper) => DVec,
                             torque: (Long, EvolutionHelper) => Double,
                             onCollision: () => Unit)
