package com.github.dunnololda.scageprojects.orbitalkiller.physics

import com.github.dunnololda.scage.ScageLibD._
import com.github.dunnololda.scage.support.DVec
import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{Vector2f, Body => Phys2dBody, BodyList => Phys2dBodyList, Collider => Phys2dCollider, DynamicShape => Phys2dDynamicShape, Shape => Phys2dShape, StaticBody => Phys2dStaticBody}
import com.github.dunnololda.scageprojects.orbitalkiller.physics.collisions.Shape

/**
  * Created by andrey on 1/7/18.
  */
object Implicits {

  implicit class Phys2dBody2BodyState(pb: Phys2dBody) {
    def toBodyState: Option[BodyState] = {
      pb.getUserData match {
        case (index: Int, shape: Shape) =>
          Some(BodyState(
            index = index,
            mass = pb.getMass,
            acc = DVec.dzero,
            vel = DVec(pb.getVelocity.getX, pb.getVelocity.getY),
            coord = DVec(pb.getPosition.getX, pb.getPosition.getY),
            ang_acc = 0.0,
            ang_vel = pb.getAngularVelocity.toDeg,
            ang = pb.getRotation.toDeg,
            shape = shape,
            is_static = pb.isStatic
          ))
        case _ => None
      }
    }
  }

  implicit class Phys2dBodyList2List(pbl: Phys2dBodyList) {
    def toList: List[(Phys2dBody, BodyState)] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield (pb, bs)).toList
    }

    def toBodyStateList: List[BodyState] = {
      (for {
        i <- 0 until pbl.size()
        pb = pbl.get(i)
        bs <- pb.toBodyState
      } yield bs).toList
    }
  }

  implicit class DVec2DoublePhys2dVector(v: DVec) {
    def toPhys2dVecD = new Vector2f(v.x, v.y)
  }
}
