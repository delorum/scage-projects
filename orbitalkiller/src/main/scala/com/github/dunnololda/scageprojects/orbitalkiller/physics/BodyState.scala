package com.github.dunnololda.scageprojects.orbitalkiller.physics

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.{Contact, Shape}

case class BodyState(index: Int,
                     mass: Double,
                     acc: DVec = DVec.zero,
                     vel: DVec = DVec.zero,
                     coord: DVec,
                     ang_acc: Double = 0,
                     ang_vel: Double = 0,
                     ang: Double = 0,
                     shape: Shape,
                     is_static: Boolean = false,
                     restitution: Double = 0.2, // elasticity or restitution: 0 - inelastic, 1 - perfectly elastic, (va2 - vb2) = -e*(va1 - vb1)
                     staticFriction: Double = 0.5,
                     dynamicFriction: Double = 0.3,
                     collisions: List[Contact] = Nil,
                     collisions_dacc: DVec = DVec.zero,
                     collisions_dvel: DVec = DVec.zero,
                     collisions_d_ang_acc: Double = 0.0,
                     collisions_d_ang_vel: Double = 0.0,
                     is_bullet: Boolean = false) {
  val aabb = shape.aabb(coord, ang)
  val I = mass * shape.wI
  val invMass = if (is_static || mass == 0) 0 else 1.0 / mass
  val invI = if (is_static || I == 0) 0 else 1.0 / I

  def toMutableBodyState = new MutableBodyState(this)
}
