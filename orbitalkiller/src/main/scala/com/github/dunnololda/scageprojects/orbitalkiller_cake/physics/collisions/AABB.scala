package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions

import com.github.dunnololda.scage.ScageLibD.DVec

// axis-aligned bounding box
case class AABB(center: DVec, width: Double, height: Double) {
  val half_width: Double = width / 2
  val half_height: Double = height / 2

  def aabbCollision(b2: AABB): Boolean = {
    val d1 = math.abs(center.x - b2.center.x)
    (d1 < half_width + b2.half_width) && {
      val d2 = math.abs(center.y - b2.center.y)
      d2 < half_height + b2.half_height
    }
  }
}

object AABB {
  def aabbCollision(b1: AABB, b2: AABB): Boolean = {
    val d1 = math.abs(b1.center.x - b2.center.x)
    (d1 < b1.half_width + b2.half_width) && {
      val d2 = math.abs(b1.center.y - b2.center.y)
      d2 < b1.half_height + b2.half_height
    }
  }
}
