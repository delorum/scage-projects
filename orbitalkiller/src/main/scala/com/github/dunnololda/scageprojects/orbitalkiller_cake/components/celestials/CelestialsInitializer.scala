package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials

import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.Celestials.{earth, moon, sun}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware

trait CelestialsInitializer extends SystemEvolutionAware {
  systemEvolution.addBody(
    moon.currentState,
    (_, helper) => {
      helper.gravityForceFromTo(sun.index, moon.index) +
        helper.gravityForceFromTo(earth.index, moon.index)
    },
    (_, _) => {
      0.0
    }
  )

  systemEvolution.addBody(
    earth.currentState,
    (_, helper) => {
      helper.gravityForceFromTo(sun.index, earth.index) +
        helper.gravityForceFromTo(moon.index, earth.index)
    },
    (_, _) => {
      0.0
    }
  )

  systemEvolution.addBody(
    sun.currentState,
    (_, helper) => {
      helper.gravityForceFromTo(earth.index, sun.index) +
        helper.gravityForceFromTo(moon.index, sun.index)
    },
    (_, _) => {
      0.0
    }
  )

  systemEvolution.addCollisionExclusion(earth.index, moon.index)
  systemEvolution.addCollisionExclusion(earth.index, sun.index)
  systemEvolution.addCollisionExclusion(moon.index, sun.index)
}
