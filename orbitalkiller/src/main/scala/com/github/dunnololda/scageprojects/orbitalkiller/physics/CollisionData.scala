package com.github.dunnololda.scageprojects.orbitalkiller.physics

import com.github.dunnololda.scage.ScageLibD._

case class CollisionData(collided_body: BodyState,
                         contact_point: DVec,
                         normal: DVec,
                         separation: Double,
                         contacts: List[net.phys2d.raw.Contact], new_velocity: DVec, new_angular_velocity: Double)
