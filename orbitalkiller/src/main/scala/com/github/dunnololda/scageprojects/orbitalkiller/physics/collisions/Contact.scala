package com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.physics.BodyState

case class Contact(a: BodyState, b: BodyState, contact_point: DVec, normal: DVec, separation: Double)
