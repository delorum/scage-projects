package com.github.dunnololda.scageprojects.orbitalkiller.colliders

import com.github.dunnololda.scageprojects.orbitalkiller.{Contact, MutableBodyState}

trait MyCollider {
  def collide(b1:MutableBodyState, b2:MutableBodyState):Contact
}
