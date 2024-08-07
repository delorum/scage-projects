package com.github.dunnololda.scageprojects.orbitalkiller_cake.util.physics

import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.state.BodyState

object PhysicsUtils {

  def curvatureRadiusInPoint(body_state: BodyState): Double = {
    math.abs(body_state.vel.norma2 / (body_state.acc * body_state.vel.p))
  }
}
