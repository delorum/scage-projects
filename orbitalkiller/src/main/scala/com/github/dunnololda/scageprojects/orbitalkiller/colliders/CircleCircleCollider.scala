package com.github.dunnololda.scageprojects.orbitalkiller.colliders

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scageprojects.orbitalkiller.{CircleShape, Contact, MutableBodyState}

object CircleCircleCollider extends MyCollider {
  private def touches(center1:DVec, radius1:Double, center2:DVec, radius2:Double):Boolean = {
    val total_radius = radius1 + radius2
    val dx = math.abs(center2.x - center1.x)
    val dy = math.abs(center2.y - center1.y)
    dx <= total_radius &&
    dy <= total_radius && {
      val total_radius_2 = total_radius*total_radius
      total_radius_2 >= dx*dx + dy*dy
    }
  }

  override def collide(b1: MutableBodyState, b2: MutableBodyState): Option[Contact] = {
    if(b1.aabb.aabbCollision(b2.aabb)) {
      val c1 = b1.body.shape.asInstanceOf[CircleShape]
      val c2 = b2.body.shape.asInstanceOf[CircleShape]

      if(touches(b1.coord, c1.radius, b2.coord, c2.radius)) {
        val normal = b2.coord - b1.coord
        val sep = c1.radius + c2.radius - normal.norma
        val pt = c1.radius*normal.n + b1.coord
        Some(Contact(b1, b2, List(pt), normal.n, -sep))
      } else {
        None
      }
    } else {
      None
    }
  }
}
