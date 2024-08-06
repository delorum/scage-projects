package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions

import com.github.dunnololda.scage.ScageLibD.DVec

// axis-aligned bounding box
case class AABB(center: DVec, width: Double, height: Double) {
  private val half_width: Double = width / 2
  private val half_height: Double = height / 2

  def collision(b2: AABB): Boolean = {
    val d1 = math.abs(center.x - b2.center.x)
    (d1 < half_width + b2.half_width) && {
      val d2 = math.abs(center.y - b2.center.y)
      d2 < half_height + b2.half_height
    }
  }
}
