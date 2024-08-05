package com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.collisions.contacts

import com.github.dunnololda.scage.ScageLibD.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.BodyState

case class Contact(a: BodyState, b: BodyState, contact_point: DVec, normal: DVec, separation: Double)
